{-# LANGUAGE BangPatterns, ScopedTypeVariables, GADTs #-}
--
-- Copyright (c) 2009 Alex Mason - http://axman6.homeip.net/blog/
-- BSD licence - http://www.opensource.org/licenses/bsd-license.php
--

-- |AVars are a form of transactional variables. They internally use a tail
-- recursive function to carry the 'state' of the variable, and allow for
-- use in concurrent systems, where actions are guaranteed to happen. They
-- are designed to cope with exceptions thrown by any modifying functions;
-- any exception thrown during a transaction will either be passed back to
-- the caller or ignored, and the variable keeps on running.
--
-- They are handy for applications like keeping track of resources by
-- incrementing and decrementing the variable. They should not be used in
-- a way which you would read the variable, then modify it based on the
-- result recieved, but rather using the provided functions. If this was
-- not done, the variable's value is very likely to have changed in the
-- mean time.
--
-- If you're after a more unsafe interface to AVars, see Data.AVar.Unsafe,
-- which will throw the errors returned fromt he variable.

module Data.AVar (
    AVar,
    putAVar,
    modAVar,
    modAVar',
    justModAVar,
    getAVar,
    condModAVar,
    swapAVar) where
import Data.AVar.Internal
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Control.Exception as E


-- |'getAVar' reads the current value inside the AVar.
getAVar :: AVar a -> IO a
getAVar (AVar chan)   = do
    res <- newEmptyMVar
    writeChan chan (Get res)
    takeMVar res


-- |'putAVar' replaces the currect value in the variable with the given x
putAVar :: AVar a -> a -> IO ()
putAVar (AVar chan) x = writeChan chan (Put x)

-- |'modAVar' takes a function from a to a, and returns Nothing if nothing went
-- wrong, or Just e, where e is an exception thrown by the function.
modAVar :: AVar a -> (a -> a) -> IO (Maybe E.SomeException)
modAVar (AVar chan) f = do
    res <- newEmptyMVar
    writeChan chan (Mod f res)
    takeMVar res

-- |'modAVar'' is like modAVar, but it modifies the variable, along with
-- returning a result of type b, within an Either e b.
modAVar' :: AVar a -> (a -> (a,b)) -> IO (Either E.SomeException b)
modAVar' (AVar chan) f = do
    res <- newEmptyMVar
    writeChan chan (Mod' f res)
    takeMVar res

-- |'justModAVar' will attempt to run the given function on the variable.
-- It does not report back on its sucess or failure, and if the function
-- produces an exception, the variable is left unchanged. It should be
-- used when you just want to modify the variable, and keep running,
-- without waiting for the action to complete.
justModAVar :: AVar a -> (a -> a) -> IO ()
justModAVar (AVar chan) f = writeChan chan (JustMod f)

-- |'condModAVar' applies the first finction to the current value in the
-- AVar, and if true will modify the value using the second function if
-- it results in True, or the third function if it results in Fasle.
condModAVar :: AVar a
            -> (a -> Bool)
            -> (a -> a)
            -> (a -> a)
            -> IO (Either E.SomeException Bool)
condModAVar (AVar chan) p t f = do
    res <- newEmptyMVar
    writeChan chan (Atom p t f res)
    takeMVar res

-- |'swapAVar' takes a new value, puts it into the AVar, and returns the old value.
swapAVar :: (AVar a) -> a -> IO (Either E.SomeException a)
swapAVar (AVar chan) new = do
    res <- newEmptyMVar
    writeChan chan (Mod' (\old -> (new, old)) res)
    takeMVar res





