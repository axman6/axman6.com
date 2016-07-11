module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Network
import Network.Curl
import Network.HTTP
import Network.HTTP.Server
import Network.Proxy.Tracker
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment
import Text.Printf


main = do
    -- crl <- initialize
    args <- getArgs
    rets <- forM args  $ \arg ->do
        ret <- newEmptyMVar
        forkIO $ do
            (CurlResponse ccode stat statln hdrs bdy info) <- curlGetResponse_ arg []
            L8.writeFile (reverse . takeWhile (/= '/') . ("tad." ++) . reverse $ arg) bdy
            printf "Path: %s stat: %d statln: %s headers: %s\n"
                arg stat statln . show $ (hdrs::[(String,String)])
            putStrLn ("Wrote: " ++ arg)
            putMVar ret ()
        return ret
    mapM_ takeMVar rets