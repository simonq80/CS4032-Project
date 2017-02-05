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
import DirectoryAPI
import FileAPI
import Data.Bson.Generic

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
    doDBRequest p (insert "users" (toBSON (SecurityUser "simon" "asdf")))
    (Just r) <- doDBRequest p (findOne $ select [] "users")
    putStrLn $ show $ head r
    putStrLn $ show $ head $ tail r
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
    doDBRequest p (insert "users" (toBSON u))
    return True


readF :: FileDetails -> Handler FileDetails
readF fd = do
    x <- liftIO $ readFIO fd
    return x

writeF :: FileDetails -> Handler Bool
writeF _ = return True


readFIO :: FileDetails -> IO FileDetails
readFIO fd = do 
    (Just sd) <- securityLogin
    (Just fl) <- getFileLoc fd sd
    (Just nfd) <- readFileIn fd fl
    return nfd





-- authenticates with the security server, getting token
securityLogin :: IO (Maybe ServerDetails)
securityLogin = do
    p <- pipe
    (Just r) <- doDBRequest p (findOne $ select [] "users")
    manager <- newManager defaultManagerSettings
    res <- runClientM (loginQuery $ getFromBSON r)(ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
    case res of
        Left err -> return Nothing
        Right r -> return r
    where   getFromBSON r = case fromBSON r of
                            Just u -> u
                            Nothing -> SecurityUser "" ""

loginQuery :: SecurityUser -> ClientM (Maybe ServerDetails)
loginQuery u = do
    q <- loginSecurityUser u
    return q


-- gets file location from directory server
getFileLoc :: FileDetails -> ServerDetails -> IO (Maybe ServerDetails)
getFileLoc f s = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (fileLocQuery (f, s))(ClientEnv manager (BaseUrl Http "localhost" 8082 ""))
    case res of
        Left err -> return Nothing
        Right r -> return r

fileLocQuery :: (FileDetails, ServerDetails)-> ClientM (Maybe ServerDetails)
fileLocQuery u = do
    q <- getFileLocation u
    return q


--reads file from file server
readFileIn :: FileDetails -> ServerDetails -> IO (Maybe FileDetails)
readFileIn f s = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (fileReadQuery (f, s))(ClientEnv manager (BaseUrl Http "localhost" 8083 ""))
    case res of
        Left err -> return Nothing
        Right r -> return r

fileReadQuery :: (FileDetails, ServerDetails) -> ClientM (Maybe FileDetails)
fileReadQuery u = do
    q <- FileAPI.readFile u
    return q
