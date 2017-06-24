{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import           Data.Aeson                     as Aeson (FromJSON, ToJSON,
                                                          decode,
                                                          defaultOptions,
                                                          encode)
import qualified Data.Maybe                     as Maybe
import qualified Data.Monoid                    as Monoid (mappend)
import qualified Data.Set                       as Set
import qualified Data.String.Conversions        as Conversions (cs)
import           Data.Text                      as Text
import qualified Data.Text.IO                   as Text.IO
import           GHC.Generics                   as Generics (Generic)
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe


-- TYPES

data State =
  State { clients :: Set.Set Client
        , survey  :: Survey
        }


data Client =
  Client { clientId :: Text
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

instance Eq Client where (Client a _) == (Client b _) = a == b
instance Ord Client where compare (Client a _) (Client b _) = compare a b
instance Show WS.Connection where show _ = show ""
instance ToJSON Survey
instance ToJSON Question
instance ToJSON Option
instance FromJSON Survey
instance FromJSON Question
instance FromJSON Option


initialState =
  State { clients = Set.empty
              , survey = initialSurvey
              }

initialSurvey =
  Survey { title = ""
         , description = ""
         , questions = []
         }


-- SERVER

runServer :: Int -> IO ()
runServer port = do
  serverState <- Concurrent.newMVar initialState
  Warp.run port $ WS.websocketsOr WS.defaultConnectionOptions
    (wsApp serverState)
    httpApp

  -- WS.runServer ip port $ application serverState

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "API pending"

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp server pending = do
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
            register conn server client
            talk conn server client
      where
        -- TODO: Standardise the instruction interface, put it in JSON.
        actionPrefixes  = [joinAsPrefix]
        joinAsPrefix    = "register as "
        client          = Client (Text.drop (Text.length joinAsPrefix) msg) conn


register :: WS.Connection -> Concurrent.MVar State -> Client -> IO ()
register conn serverState client =
  Concurrent.modifyMVar_ serverState $ \state -> do
    let newClients = Set.insert client (clients state)
    let newState = State newClients $ survey state

    WS.sendTextData conn $ Aeson.encode $ survey newState

    return newState


talk :: WS.Connection -> Concurrent.MVar State -> Client -> IO ()
talk conn serverState client = Monad.forever $ do
    msg <- WS.receiveData conn

    -- TODO: I don't know if this is actually safe:
    Concurrent.modifyMVar_ serverState $ \s -> do
      let newSurvey = updateSurvey s msg
      let newState = State (clients s) newSurvey
      return newState

    -- TODO: Figure out how to do this nicely:
    newState <- Concurrent.readMVar serverState

    Concurrent.readMVar serverState
      >>= broadcast (encodeSurvey $ survey newState)


broadcast :: Text -> State -> IO ()
broadcast message serverState =
  Monad.forM_ (clients serverState) $ \client -> WS.sendTextData (conn client) message


updateSurvey :: State -> Text -> Survey
updateSurvey serverState msg =
  Maybe.fromMaybe (survey serverState) $ decodeSurvey $ Conversions.cs msg


decodeSurvey :: Text -> Maybe Survey
decodeSurvey text =
  Aeson.decode $ Conversions.cs text


encodeSurvey :: Survey -> Text
encodeSurvey survey =
  Conversions.cs $ Aeson.encode survey


disconnect :: Concurrent.MVar State -> Client -> IO ()
disconnect serverState client =
    Concurrent.modifyMVar_ serverState $ \state ->
      return $ State (Set.delete client $ clients state) $ survey state
