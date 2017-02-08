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



pipe :: IO Pipe
pipe = connect $ Host "database" (PortNumber 27017)

doDBRequest p req = do
    access p master "main" req

startApp :: IO ()
startApp = do
    p <- pipe
    doDBRequest p (insert "files" (toBSON (FileDetails "1234" "asdf.txt" "")))
    doDBRequest p (insert "tokens" (toBSON (ServerDetails "" "" "token1")))
    run 8083 app



app :: Application
app = serve fileAPI server

server :: Server FileAPI
server = doReadFile :<|> doWriteFile :<|> doAddToken


doReadFile :: (FileDetails, ServerDetails) -> Handler (Maybe FileDetails)
doReadFile a = do
    x <- liftIO $ doReadFileIO a
    return x

doReadFileIO :: (FileDetails, ServerDetails) -> IO (Maybe FileDetails)
doReadFileIO (fd, sd) = do 
    p <- pipe
    tokenvalid <- doDBRequest p (count $ select ["token" =: (token sd)] "tokens")
    case tokenvalid of
        0 -> return Nothing
        _ -> do
            r <- doDBRequest p (findOne $ select ["filename" =: (filename fd)] "files")
            case r of
                Nothing -> return Nothing
                Just x -> return $ fromBSON x



doWriteFile :: (FileDetails, ServerDetails) -> Handler Bool
doWriteFile a = do
    x <- liftIO $ doWriteFileIO a
    return x

doWriteFileIO :: (FileDetails, ServerDetails) -> IO Bool
doWriteFileIO (fd, sd) = do
    p <- pipe
    doDBRequest p (delete $ select ["filename" =: (filename fd)] "files")
    doDBRequest p (insert "files" (toBSON fd))
    return True


doAddToken :: ServerDetails -> Handler Bool
doAddToken a = do
    x <- liftIO $ doAddTokenIO a
    return x

doAddTokenIO :: ServerDetails -> IO Bool
doAddTokenIO sd = do
    p <- pipe
    doDBRequest p (insert "tokens" (toBSON sd))
    return True



