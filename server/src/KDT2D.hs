{-# LANGUAGE BangPatterns #-}

module KDT2D
( XTree
, TreeVal(..)
, empty
, make
) where

data XTree n a = XFork !n !(YTree n a) !(YTree n a) !(YTree n a) | XLeaf ![TreeVal n a]
data YTree n a = YFork !n !(XTree n a) !(XTree n a) !(XTree n a) | YLeaf ![TreeVal n a]

data TreeVal n a = Circle    {_val :: !a, _x, _y, _r :: !n} 
                 | Rectangle {_val :: !a, _x, _y, _xr, _yr :: !n}
                 | Triangle  {_val :: !a, _xa, _ya, _xb, _yb, _xc, _yc :: !n}

empty :: XTree n a
empty = XLeaf []

make :: (Floating n, Ord n) => [TreeVal n a] -> XTree n a
make [] = empty
make items = makeX (mean 0 0 items) items
    where

    makeX !avg !qs = 
        if ln < 1 && rn < 1
        then XLeaf cs
        else
            if ln < 1 && cn < 1
            then XLeaf rs
            else
                if rn < 1 && cn < 1
                then XLeaf ls
                else XFork avg (makeY (la / ln) ls) (makeY (ca / cn) cs) (makeY (ra / rn) rs)
        where
        (!ls,!la,!ln,!cs,!ca,!cn,!rs,!ra,!rn) = splitX avg qs [] 0 0 [] 0 0 [] 0 0

    makeY !avg !qs = 
        if ln < 1 && rn < 1
        then YLeaf cs
        else
            if ln < 1 && cn < 1
            then YLeaf rs
            else
                if rn < 1 && cn < 1
                then YLeaf ls
                else YFork avg (makeX (la / ln) ls) (makeX (ca / cn) cs) (makeX (ra / rn) rs)
        where
        (!ls,!la,!ln,!cs,!ca,!cn,!rs,!ra,!rn) = splitY avg qs [] 0 0 [] 0 0 [] 0 0

    splitX !avg (q:qs) !ls !la !ln !cs !ca !cn !rs !ra !rn = case q of
        Circle _ x _ r -> 
            if x == avg
            then splitX avg qs ls la ln (q : cs) (ca + x) (cn + 1) rs ra rn
            else
                if x < avg
                then
                    if x + r >= avg
                    then splitX avg qs ls la ln (q : cs) (ca + x) (cn + 1) rs ra rn
                    else splitX avg qs (q : ls) (la + x) (ln + 1) cs ca cn rs ra rn
                else
                    if x - r <= avg
                    then splitX avg qs ls la ln (q : cs) (ca + x) (cn + 1) rs ra rn
                    else splitX avg qs ls la ln cs ca cn (q : rs) (ra + x) (rn + 1)
        Rectangle _ x _ xr _ ->
            if x == avg
            then splitX avg qs ls la ln (q : cs) (ca + x) (cn + 1) rs ra rn
            else
                if x < avg
                then
                    if x + xr >= avg
                    then splitX avg qs ls la ln (q : cs) (ca + x) (cn + 1) rs ra rn
                    else splitX avg qs (q : ls) (la + x) (ln + 1) cs ca cn rs ra rn
                else
                    if x - xr <= avg
                    then splitX avg qs ls la ln (q : cs) (ca + x) (cn + 1) rs ra rn
                    else splitX avg qs ls la ln cs ca cn (q : rs) (ra + x) (rn + 1)
        Triangle _ xa _ xb _ xc _ ->
            if xa == avg || xb == avg || xc == avg
            then splitX avg qs ls la ln (q : cs) (ca + (xa + xb + xc) / 3) (cn + 1) rs ra rn
            else
                if xa < avg || xb < avg || xc < avg
                then
                    if xa >= avg || xb >= avg || xc >= avg
                    then splitX avg qs ls la ln (q : cs) (ca + (xa + xb + xc) / 3) (cn + 1) rs ra rn
                    else splitX avg qs (q : ls) (la + (xa + xb + xc) / 3) (ln + 1) cs ca cn rs ra rn
                else
                    if xa <= avg || xb <= avg || xc <= avg
                    then splitX avg qs ls la ln (q : cs) (ca + (xa + xb + xc) / 3) (cn + 1) rs ra rn
                    else splitX avg qs ls la ln cs ca cn (q : rs) (ra + (xa + xb + xc) / 3) (rn + 1)
    splitX _ _ !ls !la !ln !cs !ca !cn !rs !ra !rn = (ls,la,ln,cs,ca,cn,rs,ra,rn)

    splitY !avg (q:qs) !ls !la !ln !cs !ca !cn !rs !ra !rn = case q of
        Circle _ _ y r -> 
            if y == avg
            then splitY avg qs ls la ln (q : cs) (ca + y) (cn + 1) rs ra rn
            else
                if y < avg
                then
                    if y + r >= avg
                    then splitY avg qs ls la ln (q : cs) (ca + y) (cn + 1) rs ra rn
                    else splitY avg qs (q : ls) (la + y) (ln + 1) cs ca cn rs ra rn
                else
                    if y - r <= avg
                    then splitY avg qs ls la ln (q : cs) (ca + y) (cn + 1) rs ra rn
                    else splitY avg qs ls la ln cs ca cn (q : rs) (ra + y) (rn + 1)
        Rectangle _ _ y _ yr ->
            if y == avg
            then splitY avg qs ls la ln (q : cs) (ca + y) (cn + 1) rs ra rn
            else
                if y < avg
                then
                    if y + yr >= avg
                    then splitY avg qs ls la ln (q : cs) (ca + y) (cn + 1) rs ra rn
                    else splitY avg qs (q : ls) (la + y) (ln + 1) cs ca cn rs ra rn
                else
                    if y - yr <= avg
                    then splitY avg qs ls la ln (q : cs) (ca + y) (cn + 1) rs ra rn
                    else splitY avg qs ls la ln cs ca cn (q : rs) (ra + y) (rn + 1)
        Triangle _ _ ya _ yb _ yc ->
            if ya == avg || yb == avg || yc == avg
            then splitY avg qs ls la ln (q : cs) (ca + (ya + yb + yc) / 3) (cn + 1) rs ra rn
            else
                if ya < avg || yb < avg || yc < avg
                then
                    if ya >= avg || yb >= avg || yc >= avg
                    then splitY avg qs ls la ln (q : cs) (ca + (ya + yb + yc) / 3) (cn + 1) rs ra rn
                    else splitY avg qs (q : ls) (la + (ya + yb + yc) / 3) (ln + 1) cs ca cn rs ra rn
                else
                    if ya <= avg || yb <= avg || yc <= avg
                    then splitY avg qs ls la ln (q : cs) (ca + (ya + yb + yc) / 3) (cn + 1) rs ra rn
                    else splitY avg qs ls la ln cs ca cn (q : rs) (ra + (ya + yb + yc) / 3) (rn + 1)
    splitY _ _ !ls !la !ln !cs !ca !cn !rs !ra !rn = (ls,la,ln,cs,ca,cn,rs,ra,rn)

    mean !a !n (!q:qs) = mean (a + getCenter q) (n + 1) qs
    mean !a !n _ = a / n

    getCenter (Circle _ x _ _) = x
    getCenter (Rectangle _ x _ _ _) = x
    getCenter (Triangle _ xa _ xb _ xc _) = (xa + xb + xc) / 3



{-
make :: (Floating n, Ord n, Ord a, Show a, Show n) => (a -> n) -> (a -> n) -> (a -> n) -> [a] -> Tree2D n a
make getR getX getY xs = makeFrom (empty getR getX getY) xs

makeFrom :: (Floating n, Ord n, Ord a, Show a, Show n) => Tree2D n a -> [a] -> Tree2D n a
makeFrom (Tree2D getR getX getY _) qs =
    Tree2D getR getX getY $ makeX (mean 0 0 getX qs) qs
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
                else XFork avg (makeY (la / ln) ls) (makeY (ca / cn) cs) (makeY (ra / rn) rs)
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
                else YFork avg (makeX (la / ln) ls) (makeX (ca / cn) cs) (makeX (ra / rn) rs)
        where
        (ls,la,ln,cs,ca,cn,rs,ra,rn) = split avg getY xs [] 0 0 [] 0 0 [] 0 0

    mean n a _ [    ] = a / n
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

inRange :: (Floating n, Ord n, Ord a) => Tree2D n a -> (n,n,n) -> [a]
inRange (Tree2D _ _ _ (XLeaf set)) _ = set
inRange (Tree2D getR getX getY fork) (x,y,r) =
    inRngX fork
    where
    inRngX (XFork avg left center right) =
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
    
    inRngY (YFork avg left center right) =
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

inRangeMatching :: (Floating n, Ord n, Ord a) => Tree2D n a -> (a -> Bool) -> (n,n,n) -> [a]
inRangeMatching (Tree2D getR getX getY (XLeaf xs)) f (x,y,r) = filter (\a -> isNear a && f a) xs
    where
    isNear a = (getX a - x)^(2::Int) + (getY a - y)^(2::Int) < (getR a + r)^(2::Int)
inRangeMatching (Tree2D getR getX getY fork) f (x,y,r) =
    inRngX fork
    where
    inRngX (XFork avg left center right) =
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
    
    inRngY (YFork avg left center right) =
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
-}