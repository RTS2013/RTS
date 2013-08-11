{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module KDT where

import GHC.Conc.Sync (par,pseq)

data KDT n a where
    Fork :: !n -> !(KDT n a) -> !(KDT n a) -> KDT n a
    Leaf :: ![a] -> KDT n a

instance (Show n, Show a) => Show (KDT n a) where
    show = toStr 4
        where
        toStr i (Fork n l r) = "Fork " ++ show n ++ "\n" ++ replicate i ' ' ++ toStr (i+4) l ++ "\n" ++ replicate i ' ' ++ toStr (i+4) r
        toStr _ (Leaf xs) = "Leaf " ++ show xs

makeKDT :: (Floating n, Ord n) => [a -> n] -> [a] -> KDT n a
makeKDT fs = mkKDT (cycle fs)
    where
    mkKDT (f:fs) xs =
        case ls of
            [] -> Leaf rs
            __ -> case rs of
                [] -> Leaf ls
                __ -> mkLeft `par` mkRight `pseq` Fork avg mkLeft mkRight
        where
        avg = mean 0 0 xs
        (ls,rs) = split xs [] []
        mkLeft  = mkKDT fs ls
        mkRight = mkKDT fs rs
        mean n a [    ] = a / n
        mean n a (x:xs) = mean (n+1) (a+f x) xs
        split [    ] ls rs = (ls,rs)
        split (x:xs) ls rs = 
            if f x < avg 
            then split xs (x:ls) rs 
            else split xs ls (x:rs)

searchKDT :: (Floating n, Ord n) => KDT n a -> [n -> (Bool,Bool)] -> [a]
searchKDT kdt fs = search kdt $ cycle fs
    where
    search :: (Floating n, Ord n) => KDT n a -> [n -> (Bool,Bool)] -> [a]
    search (Fork n a b) (f:fs) = case f n of
        (True,True) -> search a fs ++ search b fs
        (True,False) -> search a fs
        (False,True) -> search b fs
        (False,False) -> []
    search (Leaf xs) _ = xs

inRange :: (Floating n, Ord n) => KDT n a -> [a -> n] -> [n] -> n -> [a]
inRange kdt fs ns r = filter rangeFilter $ searchKDT kdt leftRightChecks
    where
    fns = zip fs ns
    rangeFilter a = sum (map (\(f,n) -> (f a-n)^2) fns) <= r^2
    leftRightChecks = map (\n v -> (n - r <= v, n + r >= v)) ns