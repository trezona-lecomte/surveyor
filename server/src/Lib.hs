{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import           Data.Aeson                     as Aeson (FromJSON, ToJSON,
                                                          decode,
                                                          defaultOptions,
                                                          encode)
import qualified Data.ByteString                as ByteString
import qualified Data.ByteString.Lazy           as ByteString.Lazy
import qualified Data.Maybe                     as Maybe
import qualified Data.Monoid                    as Monoid (mappend)
import qualified Data.Set                       as Set
import           Data.Text                      as Text
import qualified Data.Text.Encoding             as Encoding
import qualified Data.Text.IO                   as Text.IO
import           GHC.Generics                   as Generics (Generic)
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe
-- import qualified Data.List                      as List

data ServerState =
  ServerState { clients :: Set.Set Client
              , survey  :: Survey
              }


data Client =
  Client { userName :: Text
         , conn     :: WS.Connection
         } deriving (Show)


data Survey =
  Survey { title       :: Text
         , description :: Text
         , questions   :: [Question]
         } deriving (Generic, Show)


data Question =
  Question { questionId :: Text
           , format     :: Text
           , prompt     :: Text
           , options    :: [Option]
           } deriving (Generic, Show)


data Option =
  Option { optionId :: Text
         , text     :: Text
         } deriving (Generic, Show)


data Command =
    UpdateSurvey Text
  | Broadcast Text
  deriving (Eq, Show)

instance Show WS.Connection where
  show _ = show ""

instance Eq Client where
  (Client a _) == (Client b _) = a == b

instance Ord Client where
  compare (Client a _) (Client b _) = compare a b

instance ToJSON Survey
instance ToJSON Question
instance ToJSON Option
instance FromJSON Survey
instance FromJSON Question
instance FromJSON Option


initialServerState =
  ServerState { clients = Set.empty
              , survey = initialSurvey
              }

initialSurvey =
  Survey { title = ""
         , description = ""
         , questions = []
         }


runServer :: String -> Int -> IO ()
runServer ip port = do
  serverState <- Concurrent.newMVar initialServerState
  WS.runServer ip port $ application serverState


broadcast :: Text -> ServerState -> IO ()
broadcast message serverState =
  Monad.forM_ (clients serverState) $ \client -> WS.sendTextData (conn client) message


application :: Concurrent.MVar ServerState -> WS.ServerApp
application server pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  serverState <- Concurrent.readMVar server

  case msg of
    _   | not (Prelude.any (`Text.isPrefixOf` msg) actionPrefixes) ->
            WS.sendTextData conn ("Please register as a client first." :: Text)

        | Set.member client $ clients serverState ->
            WS.sendTextData conn ("Client already exists" :: Text)

        | otherwise -> flip Exception.finally (disconnect server client) $ do
            addClient conn server client
            talk conn server client
      where
        -- TODO: Standardise the instruction interface, put it in JSON.
        actionPrefixes  = [joinAsPrefix]
        joinAsPrefix    = "register as "
        client          = Client (Text.drop (Text.length joinAsPrefix) msg) conn


addClient :: WS.Connection -> Concurrent.MVar ServerState -> Client -> IO ()
addClient conn serverState client =
  Concurrent.modifyMVar_ serverState $ \server -> do
    let newClients = Set.insert client (clients server)
    let newServerState = ServerState newClients $ survey server

    WS.sendTextData conn $ Aeson.encode $ survey newServerState

    return newServerState


talk :: WS.Connection -> Concurrent.MVar ServerState -> Client -> IO ()
talk conn serverState client = Monad.forever $ do
    msg <- WS.receiveData conn

    -- TODO: I don't know if this is actually safe:
    Concurrent.modifyMVar_ serverState $ \s -> do
      let newSurvey = updateSurvey s msg
      let newServerState = ServerState (clients s) newSurvey
      return newServerState

    -- TODO: Figure out how to do this nicely:
    newServerState <- Concurrent.readMVar serverState

    Concurrent.readMVar serverState
      >>= broadcast (encodeSurvey $ survey newServerState)


updateSurvey :: ServerState -> ByteString.Lazy.ByteString -> Survey
updateSurvey serverState msg =
  Maybe.fromMaybe (survey serverState) (decodeSurvey msg)


encodeSurvey :: Survey -> Text
encodeSurvey survey =
  Encoding.decodeUtf8 $ ByteString.Lazy.toStrict $ Aeson.encode survey


decodeSurvey text =
  Aeson.decode text :: Maybe Survey


disconnect :: Concurrent.MVar ServerState -> Client -> IO ()
disconnect serverState client =
    Concurrent.modifyMVar_ serverState $ \s -> do
      let newClients = Set.delete client (clients s)
      let newServerState = ServerState newClients (survey s)

      broadcast (userName client `mappend` " disconnected") newServerState

      return newServerState
