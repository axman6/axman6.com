-- |Data.AVar.Unsafe has a similar interface to Data.AVar, but instead of letting
-- the user handle exceptions from Eithers, it will throw exceptions caught by
-- the variable.

module Data.AVar.Unsafe (
    AVar,
    Result(..),
    getAVar,
    putAVar,
    modAVar,
    modAVar',
    justModAVar,
    condModAVar,
    swapAVar
    ) where

import Data.AVar.Internal
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Control.Exception as E

data Result = OK

-- |'getAVar' reads the current value inside the AVar.
getAVar :: AVar a -> IO a
getAVar (AVar chan)   = do
    res <- newEmptyMVar
    writeChan chan (Get res)
    takeMVar res


-- |'putAVar' replaces the currect value in the variable with the given x
putAVar :: AVar a -> a -> IO ()
putAVar (AVar chan) x = writeChan chan (Put x)

-- |'modAVar' takes a function from a to a, and modifies the variable. It will
-- throw any exceptions caught by the variable when applying the function.
modAVar :: AVar a -> (a -> a) -> IO ()
modAVar (AVar chan) f = do
    res <- newEmptyMVar
    writeChan chan (Mod f res)
    r <- takeMVar res
    case r of
        Nothing -> return ()
        Just e  -> E.throw e

-- |'modAVar'' is like modAVar, but it modifies the variable, along with
-- returning a result of type b. It also throws any errors caugh by the variable.
modAVar' :: AVar a -> (a -> (a,b)) -> IO b
modAVar' (AVar chan) f = do
    res <- newEmptyMVar
    writeChan chan (Mod' f res)
    r <- takeMVar res
    case r of
        Right b -> return b
        Left e  -> E.throw e

-- |'justModAVar' will attempt to run the given function on the variable.
-- It does not report back on its sucess or failure, and if the function
-- produces an exception, the variable is left unchanged. It should be
-- used when you just want to modify the variable, and keep running,
-- without waiting for the action to complete.
justModAVar :: AVar a -> (a -> a) -> IO ()
justModAVar (AVar chan) f = writeChan chan (JustMod f)

-- |'condModAVar' applies the first finction to the current value in the
-- AVar, and will modify the value using the second function if
-- it results in 'True', or the third function if it results in 'Fasle'.
condModAVar :: AVar a
            -> (a -> Bool)
            -> (a -> a)
            -> (a -> a)
            -> IO Bool
condModAVar (AVar chan) p t f = do
    res <- newEmptyMVar
    writeChan chan (Atom p t f res)
    r <- takeMVar res
    case r of
        Right x -> return x
        Left e  -> E.throw e

-- |'swapAVar' takes a new value, puts it into the AVar, and returns the old value.
swapAVar :: (AVar a) -> a -> IO a
swapAVar (AVar chan) new = do
    res <- newEmptyMVar
    writeChan chan (Mod' (\old -> (new, old)) res)
    r <- takeMVar res
    case r of
        Right a -> return a
        Left e  -> E.throw e
