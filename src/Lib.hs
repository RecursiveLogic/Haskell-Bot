{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Lib ( startApp ) where

import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Exception (finally)
import Control.Lens ((^.))
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.Bits
import Data.Char (isPunctuation, isSpace)
import Data.Hashable
import Data.Monoid (mappend)
import Data.Text (Text, unpack)
import Data.Word
import Network.Wreq hiding (Auth)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets
import Network.URL
import Servant
import Wuss

baseURL :: String
baseURL = "https://discordapp.com/api"

gatewayURL :: String
gatewayURL = "gateway.discord.gg"

jsonEncoding :: String
jsonEncoding = "/?v=6&encoding=json"

newtype Snowflake = Snowflake Word64
  deriving (Eq, Ord, Num, Integral, Enum, Real, Bits, Hashable)

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

instance Show Snowflake where
  show (Snowflake a) = show a

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

getURL :: IO String
getURL = do
  r <- get $ baseURL ++ "/v6/gateway"
  return . unpack $ r ^. responseBody ^. key "url" . _String

runGateway :: ClientApp () -> IO ()
runGateway ws = do
  url <- getURL
  runSecureClient gatewayURL 443 jsonEncoding ws

application :: MVar ServerState -> ServerApp
application state pending = do
  conn <- acceptRequest pending
  forkPingThread conn 30

startApp :: IO ()
startApp = do
  state <- newMVar newServerState
  runServer "127.0.0.1" 9160 $ application state
