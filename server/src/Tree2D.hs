{-# LANGUAGE BangPatterns, Trustworthy #-}

module Tree2D
( Tree2D
, empty
, make
, makeFrom
, kNearest
, inRange
, inRangeMatching
) where

import Data.Monoid ((<>))

data Tree2D n a = Tree2D !(a -> n) !(a -> n) !(a -> n) !(XTree n a)
data XTree n a = XFork !n !Int !(YTree n a) !(YTree n a) !(YTree n a) | XLeaf ![a]
data YTree n a = YFork !n !Int !(XTree n a) !(XTree n a) !(XTree n a) | YLeaf ![a]

instance (Show n, Show a) => Show (Tree2D n a) where
    show (Tree2D _ _ _ t) = show t

instance (Show n, Show a) => Show (XTree n a) where
    show (XFork avg n a b c) = "XFork " <> show avg <> " " <> show n <> " (" <> show a <> ") (" <> show b <>  ") (" <> show c <> ")"
    show (XLeaf xs)        = "XLeaf " <> show xs

instance (Show n, Show a) => Show (YTree n a) where
    show (YFork avg n a b c) = "YFork " <> show avg <> " " <> show n <> " (" <> show a <> ") (" <> show b <>  ") (" <> show c <> ")"
    show (YLeaf xs)        = "YLeaf " <> show xs

{-# INLINE empty #-}
empty :: (Floating n, Ord n) => (a -> n) -> (a -> n) -> (a -> n) -> Tree2D n a
empty getR getX getY = Tree2D getR getX getY (XLeaf [])

{-# INLINE make #-}
make :: (Floating n, Ord n, Ord a, Show a, Show n) => (a -> n) -> (a -> n) -> (a -> n) -> [a] -> Tree2D n a
make getR getX getY xs = makeFrom (empty getR getX getY) xs

{-# INLINE makeFrom #-}
makeFrom :: (Floating n, Ord n, Ord a, Show a, Show n) => Tree2D n a -> [a] -> Tree2D n a
makeFrom (Tree2D getR getX getY _) qs = {-# SCC "makeFrom" #-}
    Tree2D getR getX getY $ makeX (mean (0::Int) 0 getX qs) qs
    where
    makeX _ [] = XLeaf []
    makeX avg xs =
        if ln < 1 && rn < 1
        then XLeaf cs
        else
            if ln < 1 && cn < 1
            then XLeaf rs
            else
                if rn < 1 && cn < 1
                then XLeaf ls
                else XFork avg 
                           (ln + cn + rn) 
                           (makeY (la / fromIntegral ln) ls) 
                           (makeY (ca / fromIntegral cn) cs) 
                           (makeY (ra / fromIntegral rn) rs)
        where
        (ls,la,ln,cs,ca,cn,rs,ra,rn) = split avg getX xs [] 0 0 [] 0 0 [] 0 0

    makeY _ [] = YLeaf []
    makeY avg xs =
        if ln < 1 && rn < 1
        then YLeaf cs
        else
            if ln < 1 && cn < 1
            then YLeaf rs
            else
                if rn < 1 && cn < 1
                then YLeaf ls
                else YFork avg 
                           (ln + cn + rn) 
                           (makeX (la / fromIntegral ln) ls) 
                           (makeX (ca / fromIntegral cn) cs) 
                           (makeX (ra / fromIntegral rn) rs)
        where
        (ls,la,ln,cs,ca,cn,rs,ra,rn) = split avg getY xs [] 0 0 [] 0 0 [] 0 0

    mean n a _ [    ] = a / fromIntegral n
    mean n a f (x:xs) = mean (n + 1) (a + f x) f xs

    split _   _ []     !ls !la !ln !cs !ca !cn !rs !ra !rn = (ls,la,ln,cs,ca,cn,rs,ra,rn)
    split !avg f (x:xs) !ls !la !ln !cs !ca !cn !rs !ra !rn =
        let v = f x in
        if v < avg 
        then 
            if v + getR x >= avg 
            then split avg f xs ls la ln (x:cs) (ca+v) (cn+1) rs ra rn
            else split avg f xs (x:ls) (la+v) (ln+1) cs ca cn rs ra rn
        else 
            if v - getR x <= avg
            then split avg f xs ls la ln (x:cs) (ca+v) (cn+1) rs ra rn
            else split avg f xs ls la ln cs ca cn (x:rs) (ra+v) (rn+1)

sortGroups :: (Ord n) => Tree2D n a -> (n,n,n) -> [a]
sortGroups (Tree2D _ _ _ (XLeaf set))   _         = set
sortGroups (Tree2D getR getX getY fork) (x,y,r) = kNearX fork
    where
    ---------------------------------------------------
    kNearX (XFork avg _ l c r) = 
        if x < avg
        then kNearY l ++ kNearY c ++ kNearY r
        else kNearY r ++ kNearY c ++ kNearY l
    kNearX (XLeaf xs) = xs
    ---------------------------------------------------
    kNearY (YFork avg _ l c r) = 
        if x < avg
        then kNearX l ++ kNearX c ++ kNearX r
        else kNearX r ++ kNearX c ++ kNearX l
    kNearY (YLeaf xs) = xs

    numX (XFork _ n _ _ _) = n
    numX (XLeaf xs)        = length xs
    numY (YFork _ n _ _ _) = n
    numY (YLeaf xs)        = length xs

kNearest :: (Ord n) => Tree2D n a -> Int -> (n,n,n) -> [a]
kNearest (Tree2D _ _ _ (XLeaf set))   k _       = take k set
kNearest (Tree2D getR getX getY fork) k (x,y,r) = kNearX fork
    where
    ---------------------------------------------------
    kNearX (XFork avg _ l c r) = 
        if numY l + numY c <= k || numY r + numY c <= k
        then kNearY l ++ kNearY c ++ kNearY r
        else 
            if x < avg
            then kNearY l
            else kNearY r
    kNearX (XLeaf xs) = xs
    ---------------------------------------------------
    kNearY (YFork avg _ l c r) = 
        if numX l + numX c <= k || numX r + numX c <= k
        then kNearX l ++ kNearX c ++ kNearX r
        else 
            if x < avg
            then kNearX l
            else kNearX r
    kNearY (YLeaf xs) = xs

    numX (XFork _ n _ _ _) = n
    numX (XLeaf xs)        = length xs
    numY (YFork _ n _ _ _) = n
    numY (YLeaf xs)        = length xs

{-# INLINE inRange #-}
inRange :: (Floating n, Ord n, Ord a) => Tree2D n a -> (n,n,n) -> [a]
inRange (Tree2D _ _ _ (XLeaf set)) _ = set
inRange (Tree2D getR getX getY fork) (x,y,r) = inRngX fork
    where
    inRngX (XFork avg _ left center right) =
        if x < avg
        then 
            if x + r >= avg
            then inRngY left <> inRngY center <> inRngY right
            else inRngY left <> inRngY center
        else
            if x - r <= avg
            then inRngY left <> inRngY center <> inRngY right
            else inRngY right <> inRngY center
    inRngX (XLeaf set) = filter isNear set
    
    inRngY (YFork avg _ left center right) =
        if y < avg
        then 
            if y + r >= avg
            then inRngX left <> inRngX center <> inRngX right
            else inRngX left <> inRngX center
        else
            if y - r <= avg
            then inRngX left <> inRngX center <> inRngX right
            else inRngX right <> inRngX center
    inRngY (YLeaf set) = filter isNear set

    isNear a = (getX a - x)^(2::Int) + (getY a - y)^(2::Int) < (getR a + r)^(2::Int)

{-# INLINE inRangeMatching #-}
inRangeMatching :: (Floating n, Ord n, Ord a) => Tree2D n a -> (a -> Bool) -> (n,n,n) -> [a]
inRangeMatching (Tree2D getR getX getY (XLeaf xs)) f (x,y,r) = filter (\a -> isNear a && f a) xs
    where
    isNear a = (getX a - x)^(2::Int) + (getY a - y)^(2::Int) < (getR a + r)^(2::Int)
inRangeMatching (Tree2D getR getX getY fork) f (x,y,r) =
    inRngX fork
    where
    inRngX (XFork avg _ left center right) =
        if x < avg
        then 
            if x + r >= avg
            then inRngY left <> inRngY center <> inRngY right
            else inRngY left <> inRngY center
        else
            if x - r <= avg
            then inRngY left <> inRngY center <> inRngY right
            else inRngY right <> inRngY center
    inRngX (XLeaf xs) = filter (\a -> isNear a && f a) xs
    
    inRngY (YFork avg _ left center right) =
        if y < avg
        then 
            if y + r >= avg
            then inRngX left <> inRngX center <> inRngX right
            else inRngX left <> inRngX center
        else
            if y - r <= avg
            then inRngX left <> inRngX center <> inRngX right
            else inRngX right <> inRngX center
    inRngY (YLeaf xs) = filter (\a -> isNear a && f a) xs

    isNear a = (getX a - x)^(2::Int) + (getY a - y)^(2::Int) < (getR a + r)^(2::Int)