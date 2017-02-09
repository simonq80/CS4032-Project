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
import APIDataTypes
import SecurityAPI
import ClientProxyAPI
import DirectoryAPI
import FileAPI
import Data.Bson.Generic
import System.Random
import System.IO.Unsafe
import Crypto.Cipher.AES
import qualified Data.ByteString as DBS


directoryServerDetails = ServerDetails "localhost" "8082" ""

serverToken :: DBS.ByteString
serverToken = "la9GkQyMK6TdCNHf"

pipe :: IO Pipe
pipe = connect $ Host "database" (PortNumber 27017)

doDBRequest p req = do
    access p master "main" req

startApp :: IO ()
startApp = do
    p <- pipe
    doDBRequest p (insert "users" (toBSON (SecurityUser "simon" "asdf")))
    run 8081 app



app :: Application
app = serve securityAPI server

server :: Server SecurityAPI
server = doLogin :<|> doAddUser :<|> doRemoveUser


doLogin :: SecurityUser -> Handler (Maybe ServerDetails)
doLogin u = do
    x <- liftIO $ doLoginIO u
    return x

doLoginIO :: SecurityUser -> IO (Maybe ServerDetails)
doLoginIO u = do
    p <- pipe
    c <- doDBRequest p (count $ select ["username" =: (username u), "password" =: (password u)] "users")
    case c of
        0 -> return Nothing
        _ -> do
            tok <- genUserToken
            etok <- encryptToken tok
            sendTokenToDir (ServerDetails "" "" etok)
            return $ Just $ ServerDetails (serverip directoryServerDetails) (serverport directoryServerDetails) etok --return directory server location

doAddUser :: SecurityUser -> Handler Bool
doAddUser u = do
    x <- liftIO $ doAddUserIO u
    return x


doAddUserIO :: SecurityUser -> IO Bool
doAddUserIO u = do
    p <- pipe
    doDBRequest p (insert "users" (toBSON u))
    return True

doRemoveUser :: SecurityUser -> Handler Bool
doRemoveUser u = do 
    x <- liftIO $ doRemoveUserIO u
    return x

doRemoveUserIO:: SecurityUser -> IO Bool
doRemoveUserIO u = do 
    p <- pipe
    doDBRequest p (delete $ select ["username" =: (username u), "password" =: (password u)] "users")
    return True

genUserToken :: IO String 
genUserToken = do 
    g <- newStdGen
    return $ take 10 $ randoms g

encryptToken :: String -> IO String
encryptToken sd = 
    return $ show $ encryptECB (initAES serverToken) (read sd)


sendTokenToDir :: ServerDetails -> IO ()
sendTokenToDir sd = do 
    manager <- newManager defaultManagerSettings
    res <- runClientM (tokenQuery (sd, sd)) (ClientEnv manager (BaseUrl Http "localhost" 8082 ""))
    return ()

tokenQuery :: (ServerDetails, ServerDetails) -> ClientM Bool
tokenQuery u = do
    q <- DirectoryAPI.addToken u
    return q

