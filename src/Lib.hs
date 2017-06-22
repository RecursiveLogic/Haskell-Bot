{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.Aeson.TH
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

startApp :: IO ()
startApp = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state
