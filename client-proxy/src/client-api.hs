{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module SecurityAPI where -- (getUsers, getUser, getPackages, Package(..))  where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client


-- The purpose of this section is to explain how to perform a REST call on a remote service fro your own Servant
-- service. This code will be called from the Handler doRESTCall in the handler set above.
--
-- We will access the REST serivice hackage.haskell.org, availabel on port 80. This service provides a set of endpoints
-- for haskell documentation. We will implemnt a single endpoint. For a more comprehensive example, see
-- https://haskell-servant.github.io/client-in-5-minutes.html.

-- First up, some data types we need to define the API call we want to maketype Username = Text

data User = User
  { username :: String
  , password  :: String
  } deriving (Eq, Show)



-- Next, the hackage API definition - this is the remote service
-- This defines the functions we  want to be able to call. That is all there is to it. We can now call these funtions,
-- passing in the apporpriate parameters, and returning the appropriate data from hackage.haskell.org.

type SecurityAPI = "users" :> Get '[JSON] [User]
             :<|> "login" :> ReqBody '[JSON] User :> Post '[JSON] Bool

securityAPI :: Proxy SecurityAPI
securityAPI = Proxy

getUsers :: ClientM  [User]
getUser :: User -> ClientM Bool

getUsers :<|> getUser = client securityAPI
