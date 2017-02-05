{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import ClientProxyAPI

--data User = User
--  { userName :: String
--  , password :: String
--  } deriving (Generic)

-- $(deriveJSON defaultOptions ''User)


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

--type API = Capture "p" String :> Get '[JSON] [User]
pipe :: IO Pipe
pipe = connect $ Host "database" (PortNumber 27017)

doDBRequest p req = do
    access p master "main" req

loginRequest :: SecurityUser -> ClientM (Maybe ServerDetails)
loginRequest u = do
    q <- loginSecurityUser u
    return q



startApp :: IO ()
startApp = do
    p <- pipe
    doDBRequest p (insert "users" ["username" =: "Simon", "hash" =:"asdf"])
    doDBRequest p (insert "users" ["username" =: "Leona", "hash" =:"qwer"])
    doDBRequest p (insert "users" ["username" =: "John", "hash" =:"zxcv"])
    run 8081 app



app :: Application
app = serve clientProxyAPI server

server :: Server ClientProxyAPI
server = setCreds :<|> readF :<|> writeF


setCreds :: SecurityUser -> Handler Bool
setCreds u = do
    x <- liftIO $ setNewUser u
    return x

setNewUser :: SecurityUser -> IO Bool
setNewUser u = do
    p <- pipe
    doDBRequest p (delete $ select [] "users")
    doDBRequest p (insert "users" ["username" =: (username u), "password" =:(password u)])
    return True


readF :: FileDetails -> Handler FileDetails
readF _ = return (FileDetails "" "" "")

writeF :: FileDetails -> Handler Bool
writeF _ = return True



{-|}

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
|-}