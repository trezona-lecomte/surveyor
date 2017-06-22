{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar,
                                     readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever, unless)
import           Data.Char          (isPunctuation, isSpace)
import           Data.Monoid        (mappend)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Debug.Trace        (trace)
import qualified Network.WebSockets as WS


type ServerState = Set.Set Client
-- type ServerState = (Set.Set Client, Survey)

data Client = Client { userName :: Text
                     , conn     :: WS.Connection
                     } deriving (Show)

data Survey = Survy { title       :: String
                    , description :: String
                    , questions   :: [Question]
                    }

data Question = Question { questionId :: String
                         , format     :: String
                         , prompt     :: String
                         , options    :: [Option]
                         }

data Option = Option { optionId :: String
                     , text     :: String
                     }

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

runServer :: String -> Int -> IO ()
runServer ip port = do
  state <- newMVar newServerState
  WS.runServer ip port $ application state

newServerState :: ServerState
newServerState = Set.empty

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \client -> WS.sendTextData (conn client) message

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  clients <- readMVar state

  T.putStrLn msg

  case msg of
    _   | not (any (`T.isPrefixOf` msg) actionPrefixes) ->
            WS.sendTextData conn ("Please register as a client first." :: Text)

        | Set.member client clients ->
            WS.sendTextData conn ("Client already exists" :: Text)

        | otherwise -> flip finally (disconnect state client) $ do
            addClient conn state client
            talk conn state client
      where
        actionPrefixes  = [joinAsPrefix]
        joinAsPrefix    = "register as "
        client          = Client (T.drop (T.length joinAsPrefix) msg) conn

addClient :: WS.Connection -> MVar ServerState -> Client -> IO ()
addClient conn state client =
  modifyMVar_ state $ \s -> do
    let s' = Set.insert client s

    unless (null s) $
      WS.sendTextData conn $
        "Connected users: " `mappend`
        T.intercalate ", " (Set.elems (Set.map userName s))

    broadcast (userName client `mappend` " joined") s'

    return s'

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state client = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        (userName client `mappend` " updated the survey: " `mappend` msg)

disconnect :: MVar (Set.Set Client) -> Client -> IO ()
disconnect state client =
    modifyMVar_ state $ \s -> do
      let newState = Set.delete client s

      broadcast (userName client `mappend` " disconnected") newState

      return newState
