{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib ( startApp ) where

import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets
import Servant

baseURL = "https://discordapp.com/api"

type Client = (Text, Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

application :: MVar ServerState -> ServerApp
application state pending = do
  conn <- acceptRequest pending
  forkPingThread conn 30

startApp :: IO ()
startApp = do
  state <- newMVar newServerState
  runServer "127.0.0.1" 9160 $ application state
  liftIO $ putStrLn "Websocket server running on port 9160"
