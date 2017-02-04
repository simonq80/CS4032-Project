{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import SecurityAPI

data User = User
  { userName :: String
  , password :: String
  } deriving (Generic)

$(deriveJSON defaultOptions ''User)


-- query to security

--type APIS = "test" :> Capture "p" String :> ReqBody '[JSON] User :> Post '[JSON] [User]

--apis :: Proxy APIS
--apis = Proxy

--secureret1 :: String -> User -> ClientM [User]

--secureret1 = client apis

--queries :: ClientM [User]
--queries = do
--    q2 <- secureret1 "asdf"
--    return q2

-- -------

type API = Capture "p" String :> Get '[JSON] [User]

apis :: Proxy securityAPI
apis = Proxy

query1 :: ClientM Bool
query1 = do
    q2 <- getSecurityUser (SecurityUser "Simon" "asdf")
    return q2



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
    res <- runClientM query1 (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
    case res of
        Left err -> return []
        Right b -> return []
