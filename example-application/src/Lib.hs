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
import qualified ClientProxyAPI as CPA
import Data.Bson.Generic


startApp :: IO ()
startApp = do
    putStrLn "Enter task:\nSet Credentials (S) / Read File (R) / Write File (W) / Exit (E)"
    c <- getLine
    case c of
        "S" -> do
            setCred
            startApp
        "R" -> do
            cRead
            startApp
        "W" -> do
            cWrite
            startApp
        "E" -> return ()
        _ -> do
            putStrLn "Invalid command"
            startApp

setCred :: IO ()
setCred = do
    putStrLn "Enter new username:"
    u <- getLine
    putStrLn "Enter new password:"
    p <- getLine
    manager <- newManager defaultManagerSettings
    res <- runClientM (setCredQuery $ SecurityUser u p)(ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
    case res of
        Right True -> putStrLn "Credentials set"
        _ -> putStrLn "An error occured setting credentials"


cRead :: IO ()
cRead = do
    putStrLn "Enter File Name:"
    n <- getLine
    putStrLn "Enter File Id (if known)"
    i <- getLine
    manager <- newManager defaultManagerSettings
    res <- runClientM (readFileQuery $ CPA.FileDetails n i "")(ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
    case res of
        Left err -> putStrLn "Error reading file"
        Right fd -> do
            writeFile (CPA.filename fd) (CPA.filecontents fd)
            putStrLn $ "file successfully copied to local"

cWrite :: IO ()
cWrite = do
    putStrLn "Enter File Path:"
    fp <- getLine
    fc <- readFile fp 
    manager <- newManager defaultManagerSettings
    res <- runClientM (writeFileQuery $ CPA.FileDetails fp "" fc)(ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
    case res of
        Right True -> putStrLn "File Sucessfully Written"
        _ -> putStrLn "Error writing file"

setCredQuery :: SecurityUser -> ClientM Bool
setCredQuery u = do
    q <- CPA.setCredentials u
    return q

readFileQuery :: CPA.FileDetails -> ClientM CPA.FileDetails
readFileQuery f = do
    q <- CPA.readFile f
    return q

writeFileQuery :: CPA.FileDetails -> ClientM Bool
writeFileQuery f = do
    q <- CPA.writeFile f
    return q



    





