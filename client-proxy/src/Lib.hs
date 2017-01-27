{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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
import qualified Network.HTTP.Client as CL

data User = User
  { userName :: String
  , password :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

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
users t = let c = encode (User {userName = "Simon", password = "asdf"}) in do
    return a

