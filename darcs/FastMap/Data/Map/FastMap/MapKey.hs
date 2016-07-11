{-# LANGUAGE TypeFamilies, CPP, FlexibleInstances, TypeSynonymInstances #-}

module Data.Map.FastMap.MapKey where

import Data.Int
import Data.Word
import Prelude hiding (lookup)


class MapKey k where
    data KMap k
    kmEmpty :: KMap k
    -- | An empty map
    kmInsert :: k -> Int -> KMap k -> KMap k
    -- | Inserts a key, Int pair that does not exist
    -- exist in the map already
    kmInsert' :: k -> Int -> KMap k -> KMap k
    -- | inserts a key, Int pair that may already exist,
    -- which replaces the current Int value
    kmGetIndex :: KMap k -> k -> Maybe Int
    -- | Returns the Int value for a specified key
    kmSize :: KMap k -> Int
    -- | Returns the size of the map
    -- kmOptimise :: KMap k -> KMap k

instance MapKey Int where
    data KMap Int = IntTip
                  | IntBin {-# UNPACK #-}!Int -- Key
                           {-# UNPACK #-}!Int -- Index
                           (KMap Int)
                           (KMap Int)
                  deriving (Eq)
    
    kmEmpty = IntTip
    kmSize IntTip = 0
    kmSize (IntBin _ _ l r) = 1 + kmSize l + kmSize r
    
    kmInsert k v (IntTip) = IntBin k v IntTip IntTip
    kmInsert k v (IntBin k' v' l r)
        = case compare k k' of
            LT -> IntBin k' v' (kmInsert k v l) r
            GT -> IntBin k' v' l (kmInsert k v r)
            _  -> error $ "KMap.kmInsert: Inserting a value that already exists: " ++ show k
    kmInsert' k v (IntTip) = IntBin k v IntTip IntTip
    kmInsert' k v (IntBin k' v' l r)
        = case compare k k' of
            LT -> IntBin k' v' (kmInsert' k v l) r
            GT -> IntBin k' v' l (kmInsert' k v r)
            _  -> IntBin k' v l r

    kmGetIndex IntTip _ = Nothing
    kmGetIndex (IntBin k' v l r) k
        = case compare k k' of
            LT -> kmGetIndex l k
            GT -> kmGetIndex r k
            _  -> Just v

instance Show (KMap Int) where
    show (IntBin k i IntTip IntTip) = show k ++ " -> " ++ show i ++ "\n"
    show (IntBin k i IntTip r) = show k ++ " -> " ++ show i ++ "\n" ++
                            (unlines . (map ('>':)) . lines . show) r
    show (IntBin k i l IntTip) = show k ++ " -> " ++ show i ++ "\n" ++
                            (unlines . (map ('>':)) . lines . show) l
    show (IntBin k i l r) = show k ++ " -> " ++ show i ++ "\n" ++
                            (unlines . (map ('|':)) . lines . show) l ++ 
                            (unlines . (map ('>':)) . lines . show) r
    show IntTip = "#"
    
-- kmOptimise IntTip = IntTip
-- kmOptimise o@(IntBin k v s l r)
--     | abs (kmSize l - kmSize r) < 2 = o
--     | sleft < sright = IntBin kright vright  * (IntBin kleft vleft sleft lleft rleft)
--     
--     where diff = sleft - sright
--           IntBin kleft  vleft  sleft  lleft  rleft = kmOptimise l
--           IntBin kright vright sright lright rright = kmOptimise r

-- macro to create specialised instances
#define mkKMap(ty, tip, bin)\
instance MapKey ty where { \
    data KMap ty = tip | bin {-# UNPACK #-}!ty {-# UNPACK #-}!Int (KMap ty) (KMap ty) deriving (Show, Eq); \
    kmEmpty = tip; \
    kmSize tip = 0;\
    kmSize (bin _ _ l r) = 1 + kmSize l + kmSize r;\
    kmInsert k v (tip) = bin k v tip tip; \
    kmInsert k v (bin k' v' l r)  = case compare k k' of { \
            LT -> bin k' v' (kmInsert k v l) r; \
             GT -> bin k' v' l (kmInsert k v r); \
             _  -> error "KMap.kmInsert: Inserting a value that already exists"; };\
    kmInsert' k v tip = bin k v tip tip;\
    kmInsert' k v (bin k' v' l r)\
        = case compare k k' of {\
            LT -> bin k' v' (kmInsert' k v l) r;\
            GT -> bin k' v' l (kmInsert' k v r);\
            _  -> bin k' v l r;};\
    kmGetIndex tip _ = Nothing; \
    kmGetIndex (bin k' v l r) k = case compare k k' of{ \
            LT -> kmGetIndex l k; \
            GT -> kmGetIndex r k; \
            _  -> Just v}}; 
            
-- instance Show (KMap ty) where { \
--     show (bin k i tip tip) = show k ++ " -> " ++ show i ++ "\n"; \
--     show (bin k i tip r) = show k ++ " -> " ++ show i ++ "\n" ++ \
--                             (unlines . (map ('>':)) . lines . show) r; \
--     show (bin k i l tip) = show k ++ " -> " ++ show i ++ "\n" ++ \
--                             (unlines . (map ('>':)) . lines . show) l; \
--     show (bin k i l r) = show k ++ " -> " ++ show i ++ "\n" ++ \
--                             (unlines . (map ('|':)) . lines . show) l ++  \
--                             (unlines . (map ('>':)) . lines . show) r; \
--     show tip = "#"}}


mkKMap(Int8,Int8Tip,Int8Bin)
mkKMap(Int16,Int16Tip,Int16Bin)
mkKMap(Int32,Int32Tip,Int32Bin)
mkKMap(Int64,Int64Tip,Int64Bin)
mkKMap(Word,WordTip,WordBin)
mkKMap(Word8,Word8Tip,Word8Bin)
mkKMap(Word16,Word16Tip,Word16Bin)
mkKMap(Word32,Word32Tip,Word32Bin)
mkKMap(Word64,Word64Tip,Word64Bin)
mkKMap(Float,FloatTip,FloatBin)
mkKMap(Double,DoubleTip,DoubleBin)
mkKMap(Char,CharTip,CharBin)






instance MapKey String where
    data KMap String = StrNode {-# UNPACK #-} !Char !(KMap String) !(KMap String) !(KMap String)
                     | StrNull {-# UNPACK #-} !Int !(KMap String)
                     | StrEnd
                      -- deriving (Show, Eq)
    kmEmpty = StrEnd
    -- with the newer value.
    -- kmInsert :: String -> Int -> KMap String -> KMap String
    kmInsert xss@(_:_)  v StrEnd              = singleton xss v
    kmInsert xss@(_:_)  v (StrNull v' rest)   = StrNull v' $ kmInsert xss v rest
    kmInsert []         v StrEnd              = StrNull v StrEnd
    kmInsert []         v (StrNode ele l e h) = StrNode ele (kmInsert [] v l) e h
    kmInsert []         v (StrNull _ rest)    = error "KMap.kmInsert: Inserting a value that already exists"
    kmInsert xss@(x:xs) v (StrNode ele l e h) =
        case compare x ele of
            LT -> StrNode ele (kmInsert xss v l) e h
            EQ -> StrNode ele l (kmInsert xs v e) h
            GT -> StrNode ele l e (kmInsert xss v h)
    
    -- kmInsert' :: String -> Int -> KMap String -> KMap String
    kmInsert' xss@(_:_)  v StrEnd              = singleton xss v
    kmInsert' xss@(_:_)  v (StrNull v' rest)   = StrNull v' $ kmInsert' xss v rest
    kmInsert' []         v StrEnd              = StrNull v StrEnd
    kmInsert' []         v (StrNode ele l e h) = StrNode ele (kmInsert [] v l) e h
    kmInsert' []         v (StrNull _ rest)    = StrNull v rest
    kmInsert' xss@(x:xs) v (StrNode ele l e h) =
        case compare x ele of
            LT -> StrNode ele (kmInsert' xss v l) e h
            EQ -> StrNode ele l (kmInsert' xs v e) h
            GT -> StrNode ele l e (kmInsert' xss v h)
    
    kmGetIndex = (!)
    kmSize = treeSize



--  Quickly build a tree without an initial tree. This should be used
-- to create an initial tree, using insert there after.
singleton :: String -> Int -> KMap String
singleton (x:xs) v = StrNode x StrEnd (singleton xs v) StrEnd
singleton []     v = StrNull v StrEnd

lookup :: String -> KMap String -> Maybe Int
lookup _ StrEnd                       = Nothing
lookup [] (StrNull v _)               = Just v
lookup [] (StrNode _ l _ _)           = lookup [] l
lookup xs (StrNull _ rest)            = lookup xs rest
lookup xss@(x:xs) (StrNode ele l e h) =
    case compare x ele of
        LT -> lookup xss l
        EQ -> lookup  xs e
        GT -> lookup xss h


(!) :: KMap String -> String -> Maybe Int
(!) = flip lookup


--  Returns the number of non-Val Elems. not exported
treeSize :: KMap String -> Int
treeSize StrEnd = 0
treeSize (StrNode _ l e h) = 1 + treeSize l + treeSize e + treeSize h
treeSize (StrNull _ rest) = treeSize rest

{-
let keys = take n $ randoms (mkStdGen 237) :: [Int]; vals = take n $ randoms (mkStdGen 27) :: [Int]; mp@(FU m vs) = (fromListU (zip keys vals)); n :: Int; n = 10000

(reverse . nubBy (\a b -> fst a == fst b) . reverse $ zip keys vals) ==  map (\k ->(k, fromJust $ Main.lookupU mp k)) keys

let keys = take n $ randoms (mkStdGen 237) :: [Int]; vals = take n $ randoms (mkStdGen 27) :: [Int]; mp@(F m vs) = (fromList (zip keys vals)); n :: Int; n = 10000

(reverse . nubBy (\a b -> fst a == fst b) . reverse $ zip keys vals) ==  map (\k ->(k, fromJust $ Main.lookup mp k)) keys


-}
