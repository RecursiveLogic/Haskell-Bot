{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Lib ( startApp ) where

import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Bits
import Data.Char (isPunctuation, isSpace)
import Data.Hashable
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Word
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets
import Servant

baseURL :: String
baseURL = "https://discordapp.com/api"

newtype Snowflake = Snowflake Word64
  deriving (Eq, Ord, Show, Num, Integral, Enum, Real, Bits, Hashable)

type Client = (Text, Connection)
type ServerState = [Client]

data Auth =
    Bot String
  | Client String
  | Bearer String

data Role = Role
  { roleID :: {-# UNPACK #-} !Snowflake
  , roleName :: String
  , roleColor :: Integer
  , roleHoist :: Bool
  , rolePos :: Integer
  , rolePerms :: Integer
  , roleManaged :: Bool
  , roleMention :: Bool
  } deriving (Eq, Show)

instance Show Auth where
  show (Bot    token) = "Bot " ++ token
  show (Client token) = token
  show (Bearer token) = "Bearer " ++ token

authToken :: Auth -> String
authToken (Bot    token) = token
authToken (Client token) = token
authToken (Bearer token) = token

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
