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
import qualified Data.Monoid                    as Monoid ((<>))
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
  stateMVar <- Concurrent.newMVar initialState
  Warp.run port $ WS.websocketsOr WS.defaultConnectionOptions
    (wsApp stateMVar)
    httpApp

  -- WS.runServer ip port $ application serverState

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "API pending"

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateMVar pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  state <- Concurrent.readMVar stateMVar

  case msg of
    _   | not (Prelude.any (`Text.isPrefixOf` msg) actionPrefixes) ->
            WS.sendTextData conn ("Please register as a client first." :: Text)

        | Set.member client $ clients state ->
            WS.sendTextData conn ("Client already exists" :: Text)

        | otherwise -> flip Exception.finally (disconnect stateMVar client) $ do
            register conn stateMVar client
            talk conn stateMVar client
      where
        -- TODO: Standardise the instruction interface, put it in JSON.
        actionPrefixes  = [joinAsPrefix]
        joinAsPrefix    = "register as "
        client          = Client (Text.drop (Text.length joinAsPrefix) msg) conn


register :: WS.Connection -> Concurrent.MVar State -> Client -> IO ()
register conn stateMVar client =
  Concurrent.modifyMVar_ stateMVar $ \state -> do
    let newClients = Set.insert client (clients state)
    let newState = State newClients $ survey state

    WS.sendTextData conn $ Aeson.encode $ survey newState

    return newState


broadcast :: State -> IO ()
broadcast state =
  Monad.forM_ (clients state) $ \client ->
    WS.sendTextData (conn client) $ encodeState state


talk :: WS.Connection -> Concurrent.MVar State -> Client -> IO ()
talk conn stateMVar client = Monad.forever $ do
    WS.receiveData conn >>= updateSurvey stateMVar
    newState <- Concurrent.readMVar stateMVar
    broadcast newState


updateSurvey :: Concurrent.MVar State -> Text -> IO ()
updateSurvey stateMVar msg =
  Concurrent.modifyMVar_ stateMVar $ \state ->
    return $ State (clients state) $ replaceSurvey state msg


replaceSurvey :: State -> Text -> Survey
replaceSurvey state msg =
  Maybe.fromMaybe (survey state) $ decodeSurvey $ Conversions.cs msg


decodeSurvey :: Text -> Maybe Survey
decodeSurvey text =
  Aeson.decode $ Conversions.cs text


encodeState :: State -> Text
encodeState state =
  Conversions.cs $ Aeson.encode $ survey state -- TODO: encode state instead of just survey


disconnect :: Concurrent.MVar State -> Client -> IO ()
disconnect stateMVar client =
    Concurrent.modifyMVar_ stateMVar $ \state ->
      return $ State (Set.delete client $ clients state) $ survey state
