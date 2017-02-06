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



pipe :: IO Pipe
pipe = connect $ Host "database" (PortNumber 27017)

doDBRequest p req = do
    access p master "main" req

startApp :: IO ()
startApp = do
    p <- pipe
    doDBRequest p (insert "servers" (toBSON (ServerDetails "localhost" "8083" "")))
    doDBRequest p (insert "files" (toBSON (FileServerDetails (FileDetails "1234" "asdf.txt" "") (ServerDetails "localhost" "8083" ""))))
    run 8082 app



app :: Application
app = serve directoryAPI server

server :: Server DirectoryAPI
server = doGetFileLocation :<|> doAddToken :<|> doPropogateWrite


doGetFileLocation :: (FileDetails, ServerDetails) -> Handler (Maybe ServerDetails)
doGetFileLocation a = do
    x <- liftIO $ doGetFileLocationIO a
    return x

doGetFileLocationIO :: (FileDetails, ServerDetails) -> IO (Maybe ServerDetails)
doGetFileLocationIO (fd, sd) = do 
    p <- pipe
    return Nothing

doAddToken :: (ServerDetails, ServerDetails) -> Handler Bool
doAddToken x = return False

doPropogateWrite :: (FileDetails, ServerDetails) -> Handler Bool
doPropogateWrite x = return False

