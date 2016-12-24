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
import Network.Wai.Application.Static
import Servant
import Servant.Utils.StaticFiles (serveDirectory)

import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.ForceSSL


type API = Raw

  -- "users" :> Get '[JSON] [User]
  -- :<|> Raw

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
  logStdout
  . forceSSL
  . gzip def{gzipFiles = GzipCacheFolder "tmp"}
  . autohead

api :: Proxy API
api = Proxy

server :: Server API
server = staticApp (defaultWebAppSettings "_site")
  { ssListing = ssListing (defaultFileServerSettings "_site")
  , ssIndices = ssIndices (defaultFileServerSettings "_site")
  }
  -- serveDirectory "_site"
