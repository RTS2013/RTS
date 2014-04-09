module KDTree
    ( KDTree
    , empty
    , make
    , makeFrom
    , nearby
    , nearbyMatching
    ) where

import Set (Set)
import Set
import Lazy.Stream as Z


data KDTree a = KDTree [a -> Float] (a -> Float) (KDT a)


data KDT a = Fork Float (KDT a) (KDT a) | Leaf (Set a)


empty : (comparable -> Float) -> [comparable -> Float] -> KDTree comparable
empty radius getters = KDTree getters radius <| Leaf Set.empty


make : (comparable -> Float) -> [comparable -> Float] -> [comparable] -> KDTree comparable
make radius getters entities = 
    let gs = Z.cycle (head getters) (tail getters)
        mean n a f xs = case xs of
            (x::xs) -> mean (n + 1) (a + f x) f xs
            []      -> a / n

        split avg f xs ls rs la ln ra rn = 
            case xs of
                []      -> (ls,la,ln,rs,ra,rn)
                (x::xs) -> 
                    let v = f x in
                    if v == avg
                    then split avg f xs (x::ls) (x::rs) (la+v) (ln+1) (ra+v) (rn+1) 
                    else if v < avg 
                         then if v + radius x >= avg 
                              then split avg f xs (x::ls) (x::rs) (la+v) (ln+1) (ra+v) (rn+1) 
                              else split avg f xs (x::ls) rs (la+v) (ln+1) ra rn
                         else if v - radius x <= avg
                              then split avg f xs (x::ls) (x::rs) (la+v) (ln+1) (ra+v) (rn+1) 
                              else split avg f xs ls (x::rs) la ln (ra+v) (rn+1)
        mkKDT gets (ls,la,ln,rs,ra,rn) pln prn = 
            let (f,fs) = (Z.head gets, Z.tail gets) in
            if ln <= 1
            then if rn <= 1 
                 then if ln == 1 && rn == 1
                      then if ls == rs
                           then Leaf <| Set.fromList ls
                           else Fork ((la+ra)/2) (Leaf <| Set.fromList ls) (Leaf <| Set.fromList rs)
                      else if ln == 0
                           then Leaf <| Set.fromList rs
                           else Leaf <| Set.fromList ls
                 else let lKDT = (mkKDT fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                          rKDT = (mkKDT fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn) in
                      Fork ((la+ra) / (ln+rn)) lKDT rKDT
            else if rn == 0
                 then Leaf <| Set.fromList ls
                 else if pln == ln && prn == rn 
                      then if ls == rs
                           then Leaf <| Set.fromList ls
                           else Fork ((la+ra)/2) (Leaf <| Set.fromList ls) (Leaf <| Set.fromList rs)
                      else let lKDT = (mkKDT fs (split (la/ln) f ls [] [] 0 0 0 0) ln rn)
                               rKDT = (mkKDT fs (split (ra/rn) f rs [] [] 0 0 0 0) ln rn) in
                            Fork ((la+ra) / (ln+rn)) lKDT rKDT in
    KDTree getters radius <| 
        mkKDT (Z.tail gs)
              (split (mean 0 0 (Z.head gs) entities) 
                     (Z.head gs) 
                     entities 
                     [] [] 0 0 0 0) 
              (-1) 
              (-1)


makeFrom : KDTree comparable -> [comparable] -> KDTree comparable
makeFrom (KDTree getters radius _) xs = make radius getters xs


nearby : KDTree comparable -> Float -> [Float] -> Set comparable
nearby (KDTree getters _ kdt) r ns =
    let dims = toFloat <| length ns
        fns = zip getters ns
        rangeFilter a = sum (map (\(f,n) -> (f a-n)^2) fns) <= r^dims
        leftRightChecks = map (\n v -> (n - r <= v, n + r >= v)) ns
        search kdt gets = case kdt of
            (Fork n a b) -> case (Z.head gets) n of
                (True,True) -> search a (Z.tail gets) `Set.union` search b (Z.tail gets)
                (True,False) -> search a (Z.tail gets)
                (False,True) -> search b (Z.tail gets)
                (False,False) -> Set.empty
            (Leaf xs) -> Set.foldl (\a ys -> if rangeFilter a then Set.insert a ys else ys) Set.empty xs in
    search kdt <| Z.cycle (head leftRightChecks) (tail leftRightChecks) 
     
   
nearbyMatching : KDTree comparable -> (comparable -> Bool) -> Float -> [Float] -> Set comparable
nearbyMatching (KDTree getters _ kdt) predicate r ns =
    let dims = toFloat <| length ns
        fns = zip getters ns
        rangeFilter a = sum (map (\(f,n) -> (f a-n)^2) fns) <= r^dims
        leftRightChecks = map (\n v -> (n - r <= v, n + r >= v)) ns
        search kdt gets = case kdt of
            (Fork n a b) -> case (Z.head gets) n of
                (True,True) -> search a (Z.tail gets) `Set.union` search b (Z.tail gets)
                (True,False) -> search a (Z.tail gets)
                (False,True) -> search b (Z.tail gets)
                (False,False) -> Set.empty
            (Leaf xs) -> Set.foldl 
                (\a ys -> if rangeFilter a && predicate a then Set.insert a ys else ys) Set.empty xs in
    search kdt <| Z.cycle (head leftRightChecks) (tail leftRightChecks) 