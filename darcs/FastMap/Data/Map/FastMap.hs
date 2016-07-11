{-# LANGUAGE TypeFamilies, CPP, ExistentialQuantification, RankNTypes, FlexibleContexts, FlexibleInstances #-}
module Data.Map.FastMap where

import Data.Int
import Data.Word
import Control.Monad
import Data.Monoid
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as U

import Data.Map.FastMap.MapKey


-- Boxed FastMaps

data FastMap k v = F (KMap k)       -- ^ Index mapping
                     Int            -- ^ The index of the highest value in the array
                     (V.IOVector v) -- ^ heap of values
                     -- deriving (Show,Eq)


-- instance (Show v, Show (KMap k)) => Show (FastMap k v) where
--     show (F m vs) = "F (" ++ show m ++ ") (" ++ show vs ++ ")"
-- 
-- | An empty FastMap
empty :: MapKey k => IO (FastMap k v)
empty = F kmEmpty 0 `fmap` V.new 10
-- 
-- | Inserts a key value pair into a FastMap
insert :: MapKey k => FastMap k v -> k -> v -> IO (FastMap k v)
insert (F m top vs) k v
    = case kmGetIndex m k of
        Just i  -> F m top `fmap` (updateHeap i v vs)
        Nothing -> F m' top' `fmap` addToHeap top' v vs
    where m'         = kmInsert k top' m
          top'       = succ top
          

-- | Add a value to a heap
addToHeap :: Int -> v -> V.IOVector v -> IO (V.IOVector v)
addToHeap i v arr
  = if i < len
      then do
        V.write arr i v 
        return arr
      else do
        V.grow arr (len+len)
        V.write arr i v
        return arr
  where len = V.length arr

-- | Update a value in the heap
updateHeap :: Int -> v -> V.IOVector v -> IO (V.IOVector v)
updateHeap i v arr = V.write arr i v >> return arr
-- updateHeap i v arr = V.update arr (V.singleton (i,v))

-- | Takes a list of key values pairs, and creates a FastMap from them
fromList :: MapKey k => [(k,v)] -> IO (FastMap k v)
fromList xs = do
    (top,arr) <- fromListV vals
    return (F (foldl (\m' (k,i) -> kmInsert' k i m') kmEmpty (zip keys [0..])) top arr)
    where keys = map fst xs
          vals = map snd xs

fromListV :: [v] -> IO (Int, V.IOVector v)
fromListV xs = do
  let len = length xs
  arr <- V.new len
  forM_ (zip [0..] xs) $ \(i,x) -> do
    V.write arr i x
  return (len,arr)

-- | looks for the value for a given key
lookup :: MapKey k => FastMap k v -> k -> IO (Maybe v)
lookup (F m _ vs) k = 
    case kmGetIndex m k of
        Nothing -> return Nothing
        Just i  -> Just `fmap` V.read vs i

-- Unboxed FastMaps

data Unbox v => FastMapU k v = FU (KMap k) -- ^ Index mapping
                    Int         -- ^ Top index in map
                     (U.IOVector v)
                     -- ^ array of values
                     -- deriving (Show,Eq)


-- instance (Show v, Show (KMap k), U.Unbox v) => Show (FastMapU k v) where
--     show (FU m vs) = "F (" ++ show m ++ ") (" ++ show vs ++ ")"

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

