{-# LANGUAGE Trustworthy #-}

module KDT (KDT,make,nearby,empty,main) where
import qualified Data.Set as Set
import qualified System.Random as R
import qualified Criterion.Main as C
import Control.Parallel.Strategies (rpar,rseq,runEval)

main :: IO ()
main = do
    xs <- fmap (R.randomRs (0,2236::Double)) R.getStdGen
    ys <- fmap (R.randomRs (0,2236::Double)) R.getStdGen
    C.defaultMain [C.bench "KDT" $ C.whnf (make (\(_,_,r) -> r) [\(x,_,_) -> x, \(_,y,_) -> y]) $ take 5000 $ zip3 xs ys (cycle [0.25])]


data KDT n a = Fork !n !(KDT n a) !(KDT n a) | Leaf ![a]

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

empty :: (Floating n, Ord n) => KDT n a
empty = Leaf []

make :: (Floating n, Ord n, Eq a) => (a -> n) -> [a -> n] -> [a] -> KDT n a
make _ [] _ = Leaf []
make radius getters entities = mkKDT (2::Int) 
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
    mkKDT 0 (f:fs) (ls,la,ln,rs,ra,rn) pln prn =
        if ln <= 1
        then if rn <= 1 
             then if ln == 1 && rn == 1
                  then if ls == rs
                       then Leaf ls
                       else Fork ((la+ra)/2) (Leaf ls) (Leaf rs)
                  else if ln == 0
                       then Leaf rs
                       else Leaf ls
             else let lKDT = (mkKDT 0 fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                      rKDT = (mkKDT 0 fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn) in
                  Fork ((la+ra) / (ln+rn)) lKDT rKDT
        else if rn == 0
             then Leaf ls
             else if pln == ln && prn == rn 
                  then if ls == rs
                       then Leaf ls
                       else Fork ((la+ra)/2) (Leaf ls) (Leaf rs)
                  else let lKDT = (mkKDT 0 fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                           rKDT = (mkKDT 0 fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn) in
                        Fork ((la+ra) / (ln+rn)) lKDT rKDT
    mkKDT n (f:fs) (ls,la,ln,rs,ra,rn) pln prn =
        if ln <= 1
        then if rn <= 1 
             then if ln == 1 && rn == 1
                  then if ls == rs
                       then Leaf ls
                       else Fork ((la+ra)/2) (Leaf ls) (Leaf rs)
                  else if ln == 0
                       then Leaf rs
                       else Leaf ls
             else runEval $ do
                      lKDT <- rpar (mkKDT (n-1) fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                      rKDT <- rseq (mkKDT (n-1) fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn)
                      _ <- rseq lKDT
                      return $ Fork ((la+ra) / (ln+rn)) lKDT rKDT
        else if rn == 0
             then Leaf ls
             else if pln == ln && prn == rn 
                  then Fork ((la+ra)/2) (Leaf ls) (Leaf rs)
                  else runEval $ do
                          lKDT <- rpar (mkKDT (n-1) fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                          rKDT <- rseq (mkKDT (n-1) fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn)
                          _ <- rseq lKDT
                          return $ Fork ((la+ra) / (ln+rn)) lKDT rKDT
    mkKDT _ [] _ _ _ = Leaf []

nearby :: (Floating n, Ord n, Ord a) => KDT n a -> [a -> n] -> [n] -> n -> Set.Set a
nearby kdt getters ns r = searchKDT leftRightChecks
    where
    dims = length ns
    fns = zip getters ns
    rangeFilter a = sum (map (\(f,n) -> (f a-n)^(2::Int)) fns) <= r^dims
    leftRightChecks = map (\n v -> (n - r <= v, n + r >= v)) ns
    searchKDT checks = search kdt $ cycle checks
        where
        search (Fork n a b) (f:fs) = case f n of
            (True,True) -> search a fs `Set.union` search b fs
            (True,False) -> search a fs
            (False,True) -> search b fs
            (False,False) -> Set.empty
        search (Leaf xs) _ = Set.fromList $ filter rangeFilter xs
        search _ _ = Set.empty