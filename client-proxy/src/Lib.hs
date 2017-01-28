{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.List.Split
import Control.Monad.IO.Class
import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import GHC.Generics
import Data.Proxy
import Servant.Client
import Servant.API

data User = User
  { userName :: String
  , password :: String
  } deriving (Generic)

$(deriveJSON defaultOptions ''User)

type APIS = Capture "p" String :> ReqBody '[JSON] User :> Post '[JSON] [User]

apis :: Proxy APIS
apis = Proxy

secureret = client apis

query1 = secureret "login" (User "Simon" "asdf")


type API = Capture "p" String :> Get '[JSON] [User]



startApp :: IO ()
startApp = do
    pipe <- connect $ Host "database" (PortNumber 27017)
    access pipe master "users" (insert "users" ["username" =: "Simon", "hash" =:"asdf"])
    access pipe master "users" (insert "users" ["username" =: "Leona", "hash" =:"qwer"])
    access pipe master "users" (insert "users" ["username" =: "John", "hash" =:"zxcv"])
    run 8081 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = userList

userList :: String -> Handler [User]
userList t = do
    u <- liftIO $ users t
    return u

users :: String -> IO [User]
users t = do
    manager <- newManager defaultManagerSettings
    res <- runClientM $ query1 (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
    case res of
        Left err -> return []
        Right user -> do
            return user

