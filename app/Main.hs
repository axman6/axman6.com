{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Servant

import           Network.HTTP.Client                  hiding (Proxy)
import           Network.HTTP.ReverseProxy

import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.ForceSSL
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger


type API = "local":> Raw :<|> Raw

startApp :: IO ()
startApp = do
  mgr <- newManager defaultManagerSettings
  runTLS
    (tlsSettings "certificate.pem" "key.pem")
    (setPort 443 defaultSettings)
    (middlewares (app mgr))
  -- startApp = run 8080 app

app :: Manager -> Application
app mgr = serve api (server mgr)


middlewares :: Middleware
middlewares =
  logStdout
  . forceSSL
  . gzip def{gzipFiles = GzipCacheFolder "tmp"}
  . autohead

api :: Proxy API
api = Proxy

server :: Manager -> Server API
server mgr =
  Tagged (staticApp (defaultWebAppSettings "_site")
        { ssListing = ssListing (defaultFileServerSettings "_site")
        , ssIndices = ssIndices (defaultFileServerSettings "_site")
        }
  )
  :<|> Tagged (waiProxyTo (\_req -> print _req >> (pure . WPRProxyDest $ ProxyDest "localhost" 8000)) defaultOnExc mgr)


main :: IO ()
main = startApp
