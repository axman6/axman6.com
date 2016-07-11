{-# LANGUAGE BangPatterns, ScopedTypeVariables, GADTs #-}

-- | The guts of how AVars work.

module Data.AVar.Internal (
    -- * Types
    AVar(..),
    Transaction(..),
    -- * functions on AVars
    newAVar,
    ) where


import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Control.Exception as E

-- |A 'Transaction' describes what should happen to a variable.
-- They are only used internally, and are here just for reference.
data Transaction a =
      Put a
    -- ^puts the a into the variable
    | Get (MVar a)
    -- ^reads the variable
    | Mod (a -> a) (MVar (Maybe E.SomeException))
    -- ^modifies the variable
    | JustMod (a -> a)
    -- ^ Just modifies the variable (unless an exception occurs)
    | forall b. Mod' (a -> (a,b)) (MVar (Either E.SomeException b))
    -- ^ modifies the variable, returning the b result to the caller
    | Atom (a -> Bool) (a -> a) (a -> a) (MVar (Either E.SomeException Bool))
    -- ^conditionally modifies a variable

    --Swap a (MVar a)

-- |'AVar's are the means through which communication with the variable are
-- conducted. They contain a Chan that is 'connected' to the variable, and
-- is read by the variable's 'handler' function.
data AVar a = AVar (Chan (Transaction a)) 

-- * Functions on AVars

-- |'newAVar' creates a new variable. It forks off the 'handler' that does the
-- work for the variable itself and creates a new AVar.
newAVar :: a -> IO (AVar a)
newAVar x = do
    E.evaluate x
    chan <- newChan :: IO (Chan (Transaction a))
    forkIO (handler chan x)
    return (AVar chan)


-- |'handler' stores the state of the variable in an accumulatin parameter.
-- It reads the chan it was forked with, and takes action depending on the
-- Transaction is recieves. Handler is not something to be used outside of
-- an AVar, and is not exported.
handler :: Chan (Transaction a) -> a -> IO b 
handler chan !x = do
  req <- readChan chan
  case req of
    Put a         -> handler chan a
    Get mvar      -> do
        putMVar mvar x
        handler chan x

    Mod f mvar    -> do
        let x' = f x
        p <- E.catch (E.evaluate x' >> return Nothing)
                     (\e -> return (Just e))
        putMVar mvar p
        case p of
          Nothing -> handler chan x'
          _       -> handler chan x
    JustMod f     -> do
        let x' = f x
        res <- E.try (E.evaluate x')
        case res of
            Right _                    -> handler chan x'
            Left  (_::E.SomeException) -> handler chan x

    Mod' f mvar    -> do
          let y@(a,b) = f x
          p <- E.try (E.evaluate a >> E.evaluate b)
          case p of
              Right _  -> do
                  putMVar mvar (Right b)
                  handler chan a
              (Left e) -> do
                  putMVar mvar (Left e)
                  handler chan x

    Atom test y n res -> do
        let t' = test x
            y' = y x
            n' = n x
        tres <- E.try (E.evaluate t')
        case tres of
            rT@(Right True) -> do
                run <- E.try (E.evaluate y')
                case run of
                    Right x' -> putMVar res rT >> handler chan x'
                    Left e   -> putMVar res (Left e) >> handler chan x
            rF@(Right False) -> do
                run <- E.try (E.evaluate n')
                case run of 
                    Right x' -> putMVar res rF >> handler chan x'
                    Left e   -> putMVar res (Left e) >> handler chan x
            Left e           -> putMVar res (Left e) >> handler chan x
        -- if test x
        --     then do
        --         putMVar res True
        --         handler chan (y x)
        --       else do
        --           putMVar res False
        --           handler chan (n x)
