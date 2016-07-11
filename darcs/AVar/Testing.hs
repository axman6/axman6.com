import Data.AVar
import Control.Monad
import System.Environment
import Control.Concurrent
import Control.Concurrent.MVar

main = do
    n <- getArgs >>= \xs -> if null xs
                    then return 1000000
                    else (readIO.head) xs 
    var <- newAVar (0 :: Integer)
    m <- newEmptyMVar
    parallel . replicate 100 $ test n var 

    getAVar var >>= print
    where test 0 _  = return ()
          test n var = do
              -- res <- getAVar var
              -- putAVar var (res + 1)
              justModAVar var (+1)
              test (n-1) var 
          parallel :: [IO a] -> IO [a]
          parallel actions = do
              vars <- forM actions $ \action -> do
                  var <- newEmptyMVar
                  forkIO $ do
                      answer <- action
                      putMVar var answer
                  return var
              forM vars takeMVar
