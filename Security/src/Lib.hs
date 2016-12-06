{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
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

data User = User
  { userName :: String
  , password :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = Capture "u" String :> Capture "p" String :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = userList

userList :: String -> String -> Handler [User]
userList u p = do
    u <- liftIO $ users u p
    return u

users :: String -> String -> IO [User]
users u p = do
    users <- readFile "/Distro/Security/users.txt"
    return $ filter (userFilter u p) (map toUser (lines users))

toUser :: String -> User
toUser s = User (head split) (head $ tail split) where
    split = splitOn ":" s

userFilter :: String -> String -> User -> Bool
userFilter u p s = (u == userName s) && (p == password s)
