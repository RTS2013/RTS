{-# LANGUAGE Trustworthy #-}

module KDTree
( KDTree
, empty
, make
, makeFrom
, nearby
, nearbyMatching
) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Parallel.Strategies (rpar, rseq, runEval)

data KDTree n a = KDTree ![a -> n] !(a -> n) !(KDT n a)

instance (Show n, Show a) => Show (KDTree n a) where
    show (KDTree _ _ kdt) = show kdt

data KDT n a = Fork !n !(KDT n a) !(KDT n a) | Leaf !(Set a)

instance (Show n, Show a) => Show (KDT n a) where
    show = toStr 4
        where
            toStr i (Fork n l r) = "Fork " 
                                ++ show n 
                                ++ "\n" 
                                ++ replicate i ' ' 
                                ++ toStr (i+4) l 
                                ++ "\n" ++ replicate i ' ' 
                                ++ toStr (i+4) r
            toStr _ (Leaf xs) = "Leaf " ++ show xs

empty :: (Floating n, Ord n) => (a -> n) -> [a -> n] -> KDTree n a
empty _ [] = error "Cannot make 0 dimensionality KD-Tree."
empty radius getters = KDTree getters radius $ Leaf Set.empty

make :: (Floating n, Ord n, Ord a) => (a -> n) -> [a -> n] -> [a] -> KDTree n a
make _ [] _ = error "Cannot make 0 dimensionality KD-Tree."
make radius getters entities = KDTree getters radius $ 
        mkKDT (20::Int) 
              (cycle $ tail getters) 
              (split (mean 0 0 (head getters) entities) 
                     (head getters) 
                     entities 
                     [] [] 0 0 0 0) 
              (-1) 
              (-1)
    where
        mean n a _ [    ] = a / n
        mean n a f (x:xs) = mean (n + 1) (a + f x) f xs

        split _ _ [] ls rs la ln ra rn = (ls,la,ln,rs,ra,rn)
        split avg f (x:xs) ls rs la ln ra rn = 
            let v = f x in
            if v == avg
            then split avg f xs (x:ls) (x:rs) (la+v) (ln+1) (ra+v) (rn+1) 
            else if v < avg 
                 then if v + radius x >= avg 
                      then split avg f xs (x:ls) (x:rs) (la+v) (ln+1) (ra+v) (rn+1) 
                      else split avg f xs (x:ls) rs (la+v) (ln+1) ra rn
                 else if v - radius x <= avg
                      then split avg f xs (x:ls) (x:rs) (la+v) (ln+1) (ra+v) (rn+1) 
                      else split avg f xs ls (x:rs) la ln (ra+v) (rn+1)

        mkKDT 0 _ (ls,_,_,rs,_,_) _ _ = Leaf . Set.fromList $ rs ++ ls
        mkKDT n (f:fs) (ls,la,ln,rs,ra,rn) pln prn =
            if ln <= 1
            then if rn <= 1 
                 then if ln == 1 && rn == 1
                      then if ls == rs
                           then Leaf $ Set.fromList ls
                           else Fork ((la+ra)/2) (Leaf $ Set.fromList ls) (Leaf $ Set.fromList rs)
                      else if ln == 0
                           then Leaf $ Set.fromList rs
                           else Leaf $ Set.fromList ls
                 else let lKDT = (mkKDT (n-1) fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                          rKDT = (mkKDT (n-1) fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn) in
                      Fork ((la+ra) / (ln+rn)) lKDT rKDT
            else if rn == 0
                 then Leaf $ Set.fromList ls
                 else if pln == ln && prn == rn 
                      then if ls == rs
                           then Leaf $ Set.fromList ls
                           else Fork ((la+ra)/2) (Leaf $ Set.fromList ls) (Leaf $ Set.fromList rs)
                      else let lKDT = (mkKDT (n-1) fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                               rKDT = (mkKDT (n-1) fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn) in
                            Fork ((la+ra) / (ln+rn)) lKDT rKDT
        {-mkKDT 0 (f:fs) (ls,la,ln,rs,ra,rn) pln prn =
            if ln <= 1
            then if rn <= 1 
                 then if ln == 1 && rn == 1
                      then if ls == rs
                           then Leaf $ Set.fromList ls
                           else Fork ((la+ra)/2) (Leaf $ Set.fromList ls) (Leaf $ Set.fromList rs)
                      else if ln == 0
                           then Leaf $ Set.fromList rs
                           else Leaf $ Set.fromList ls
                 else let lKDT = (mkKDT 0 fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                          rKDT = (mkKDT 0 fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn) in
                      Fork ((la+ra) / (ln+rn)) lKDT rKDT
            else if rn == 0
                 then Leaf $ Set.fromList ls
                 else if pln == ln && prn == rn 
                      then if ls == rs
                           then Leaf $ Set.fromList ls
                           else Fork ((la+ra)/2) (Leaf $ Set.fromList ls) (Leaf $ Set.fromList rs)
                      else let lKDT = (mkKDT 0 fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                               rKDT = (mkKDT 0 fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn) in
                            Fork ((la+ra) / (ln+rn)) lKDT rKDT
        mkKDT n (f:fs) (ls,la,ln,rs,ra,rn) pln prn =
            if ln <= 1
            then if rn <= 1 
                 then if ln == 1 && rn == 1
                      then if ls == rs
                           then Leaf $ Set.fromList ls
                           else Fork ((la+ra)/2) (Leaf $ Set.fromList ls) (Leaf $ Set.fromList rs)
                      else if ln == 0
                           then Leaf $ Set.fromList rs
                           else Leaf $ Set.fromList ls
                 else runEval $ do
                          lKDT <- rpar (mkKDT (n-1) fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                          rKDT <- rseq (mkKDT (n-1) fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn)
                          _    <- rseq lKDT
                          return $ Fork ((la+ra) / (ln+rn)) lKDT rKDT
            else if rn == 0
                 then Leaf $ Set.fromList ls
                 else if pln == ln && prn == rn 
                      then if ls == rs
                           then Leaf $ Set.fromList ls
                           else Fork ((la+ra)/2) (Leaf $ Set.fromList ls) (Leaf $ Set.fromList rs)
                      else runEval $ do
                              lKDT <- rpar (mkKDT (n-1) fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                              rKDT <- rseq (mkKDT (n-1) fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn)
                              _    <- rseq lKDT
                              return $ Fork ((la+ra) / (ln+rn)) lKDT rKDT
        -}
        mkKDT _ _ _ _ _ = error "This shouldn't have happened."

makeFrom :: (Floating n, Ord n, Ord a) => KDTree n a -> [a] -> KDTree n a
makeFrom (KDTree getters radius _) xs = make radius getters xs

nearby :: (Floating n, Ord n, Ord a) => KDTree n a -> n -> [n] -> Set a
nearby (KDTree getters radius kdt) r ns =
        if length getters == length ns
        then searchKDT leftRightChecks
        else error "Search dimensionality did not match KD-Tree dimensionality."
    where
        dims = length ns
        fns = zip getters ns
        rangeFilter a = sum (map (\(f,n) -> (f a-n)^(2::Int)) fns) <= (r+radius a)^dims
        leftRightChecks = map (\n v -> (n - r <= v, n + r >= v)) ns
        searchKDT checks = search kdt $ cycle checks
            where
            search (Fork n a b) (f:fs) = case f n of
                (True,True) -> search a fs `Set.union` search b fs
                (True,False) -> search a fs
                (False,True) -> search b fs
                (False,False) -> Set.empty
            search (Leaf xs) _ = Set.filter rangeFilter xs
            search _ _ = error "This shouldn't have happened."

nearbyMatching :: (Floating n, Ord n, Ord a) => KDTree n a -> (a -> Bool) -> n -> [n] -> Set a
nearbyMatching (KDTree getters radius kdt) predicate r ns =
        if length getters == length ns
        then searchKDT leftRightChecks
        else error "Search dimensionality did not match KD-Tree dimensionality."
    where
        dims = length ns
        fns = zip getters ns
        rangeFilter a = sum (map (\(f,n) -> (f a-n)^(2::Int)) fns) <= (r+radius a)^dims
        leftRightChecks = map (\n v -> (n - r <= v, n + r >= v)) ns
        searchKDT checks = search kdt $ cycle checks
            where
            search (Fork n a b) (f:fs) = case f n of
                (True,True) -> search a fs `Set.union` search b fs
                (True,False) -> search a fs
                (False,True) -> search b fs
                (False,False) -> Set.empty
            search (Leaf xs) _ = Set.filter (\a -> predicate a && rangeFilter a) xs
            search _ _ = error "This shouldn't have happened."