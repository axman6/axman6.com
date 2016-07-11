{-# LANGUAGE TypeFamilies, CPP, ExistentialQuantification,
             RankNTypes, FlexibleContexts, FlexibleInstances,
             NoMonomorphismRestriction #-}

import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.ST
import Control.Monad.Primitive

import MapKey

-- Boxed FastMaps

data FastMap k v = F (KMap k)       -- ^ Index mapping
                     -- !Int           -- ^ Last element in heap
                     (V.Vector v)   -- ^ Heap of values
                     -- deriving (Show)


instance (Show v, Show (KMap k)) => Show (FastMap k v) where
    show (F m vs) = "F (" ++ show m ++ ") (" ++ show vs ++ ")"
-- 
-- | An empty FastMap
empty :: MapKey k => FastMap k v
empty = F kmEmpty G.empty
--  
-- | Inserts a key value pair into a FastMap
insert :: MapKey k => FastMap k v -> k -> v -> FastMap k v
insert (F m vs) k v
    = case kmGetIndex m k of
        Just i  -> F m  (updateHeap i v vs)
        Nothing -> F m' vs'
    where (idx, vs') = addToHeap v vs
          m'         = kmInsert k idx m
           
-- 
-- | Add a value to a heap
addToHeap :: v -> V.Vector v -> (Int, V.Vector v)
addToHeap v vs = (V.length vs' - 1, vs')
    where vs' = V.snoc vs v

-- | Update a value in the heap
updateHeap :: Int -> v -> V.Vector v -> V.Vector v
updateHeap i v arr = V.update arr (V.singleton (i,v))

-- | Takes a list of key values pairs, and creates a FastMap from them
fromList :: MapKey k => [(k,v)] -> FastMap k v
fromList xs = F (foldl (\m' (k,i) -> kmInsert' k i m') kmEmpty (zip keys [0..]))
                (V.fromList vals)
    where keys = map fst xs
          vals = map snd xs

-- | looks for the value for a given key
lookup :: MapKey k => FastMap k v -> k -> Maybe v
lookup (F m vs) k = kmGetIndex m k >>= return . (vs V.!)

-- Unboxed FastMaps

-- data FastMapU k v = FU (KMap k) -- ^ Index mapping
--                      (U.Vector v)
--                      -- ^ array of values
--                      -- deriving (Show,Eq)
-- 
-- 
-- instance (Show v, Show (KMap k), U.Unbox v) => Show (FastMapU k v) where
--     show (FU m vs) = "F (" ++ show m ++ ") (" ++ show vs ++ ")"
-- 
-- emptyU :: (MapKey k, U.Unbox v) => FastMapU k v
-- emptyU = FU kmEmpty U.empty
-- 
-- insertU :: (MapKey k, U.Unbox v) => FastMapU k v -> k -> v -> FastMapU k v
-- insertU (FU m vs) k v
--     = case kmGetIndex m k of
--         Just i  -> FU m  (updateHeapU i v vs)
--         Nothing -> FU m' vs'
--     where m' = kmInsert k idx m
--           (idx, vs') = addToHeapU v vs
-- 
-- addToHeapU :: U.Unbox v => v -> U.Vector v -> (Int, U.Vector v)
-- addToHeapU v vs = (U.length vs' - 1, vs')
--     where vs' = U.snoc vs v
-- 
-- updateHeapU :: U.Unbox v => Int -> v -> U.Vector v -> U.Vector v
-- updateHeapU i v arr = U.update arr (U.singleton (i,v))
-- 
-- 
-- fromListU :: (MapKey k, U.Unbox v) => [(k,v)] -> FastMapU k v
-- fromListU xs = FU (foldl (\m' (k,i) -> kmInsert k i m') kmEmpty (zip keys [0..]))
--                 (U.fromList vals)
--     where keys = map fst xs
--           vals = map snd xs
-- 
-- lookupU :: (MapKey k, U.Unbox v) => FastMapU k v -> k -> Maybe v
-- lookupU (FU m vs) k = kmGetIndex m k >>= return . (vs U.!)

