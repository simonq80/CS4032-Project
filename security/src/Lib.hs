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


data User = User
  { userName :: String
  , password :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = Capture "p" String :> ReqBody '[JSON] User :> Post '[JSON] [User]


startApp :: IO ()
startApp = do
    pipe <- connect (host "database")
    access pipe master "users" (insert "users" ["username" =: "Simon", "hash" =:"asdf"])
    access pipe master "users" (insert "users" ["username" =: "Leona", "hash" =:"qwer"])
    access pipe master "users" (insert "users" ["username" =: "John", "hash" =:"zxcv"])
    run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = userList

userList :: String -> User -> Handler [User]
userList t usr = do
    u <- liftIO $ users t usr
    return u

users :: String -> User -> IO [User]
users t u = do
    pipe <- connect (host "database")
    c <- access pipe master "users" (count (select ["username" =: (userName u), "hash" =: (password u)] "users"))
    if (c >= 0) then return $ [User (userName u) (password u)]
                else return $ []

