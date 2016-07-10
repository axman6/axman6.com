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
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.Gzip
import Servant
import Servant.Utils.StaticFiles (serveDirectory)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
  :<|> Raw

startApp :: IO ()
startApp = runTLS
  (tlsSettings "certificate.pem" "key.pem")
  (setPort 443 defaultSettings)
  (middlewares app)
-- startApp = run 8080 app

app :: Application
app = serve api server

middlewares :: Middleware
middlewares =
  gzip def{gzipFiles = GzipCacheFolder "tmp"}

api :: Proxy API
api = Proxy

server :: Server API
server = return users :<|> serveDirectory "_site"

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
