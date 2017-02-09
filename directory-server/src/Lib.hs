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
    doDBRequest p (insert "servers" (toBSON (ServerDetails "localhost" "8083" "")))
    doDBRequest p (insert "files" (toBSON (FileServerDetails "1234" "asdf.txt" "" "localhost" "8083" "")))
    run 8082 app



app :: Application
app = serve directoryAPI server

server :: Server DirectoryAPI
server = doGetFileLocation :<|> doAddToken :<|> doPropogateWrite


doGetFileLocation :: (FileDetails, ServerDetails) -> Handler (Maybe ServerDetails)
doGetFileLocation a = do
    x <- liftIO $ doGetFileLocationIO a
    return x
    

doGetFileLocationIO :: (FileDetails, ServerDetails) -> IO (Maybe ServerDetails) --TODO: add new file case & token validation
doGetFileLocationIO (fd, sd) = do 
    p <- pipe
    r <- doDBRequest p (findOne $ select ["fileid1" =: (fileid fd)] "files")
    case r of
        Nothing -> do
            r1 <- doDBRequest p (findOne $ select ["filename1" =: (filename fd)] "files")
            case r1 of
                Nothing -> do
                    r2 <- doDBRequest p (findOne $ select [] "servers")
                    case r2 of
                        Nothing -> return Nothing
                        Just x -> return $ fromBSON x
                Just x -> case fromBSON x of
                    Nothing -> return Nothing
                    Just x -> return $ Just (ServerDetails (serverip1 x) (serverport1 x) (token1 x))
        Just y -> case fromBSON y of
            Nothing -> return Nothing
            Just x -> return $ Just (ServerDetails (serverip1 x) (serverport1 x) (token1 x))

doAddToken :: (ServerDetails, ServerDetails) -> Handler Bool
doAddToken a = do
    x <- liftIO $ doAddTokenIO a
    return x


doAddTokenIO :: (ServerDetails, ServerDetails) -> IO Bool
doAddTokenIO (sd1, sd2) = do
    p <- pipe
    doDBRequest p (insert "tokens" (toBSON sd1))
    return True


doPropogateWrite :: (FileDetails, ServerDetails) -> Handler Bool
doPropogateWrite x = return False

