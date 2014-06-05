module Pathfinding 
( Pathing
, empty
, setup
, isTileOpen
, getPath
) where

import {-------} Data.Graph.AStar (aStar)
import {-------} Control.Parallel.Strategies (withStrategy,parListChunk,rdeepseq)
import qualified Grid.Boxed   as B
import qualified Grid.Unboxed as U
import           Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

data Pathing = Pathing
    { cornerGrid :: !(B.Grid (Set (Int,Int)))
    , isOpenGrid :: !(U.Grid Bool)
    }

empty :: Pathing
empty = Pathing (B.make (0,0) Set.empty) (U.make (0,0) False)

setup :: 
    ((Int,Int) -> Bool) -> -- Node open predicate
    (Int,Int) -> -- Grid width & height
    Pathing
setup isOpen dims = Pathing cornerGrid' isOpenGrid'
    where
    corners = getCorners isOpen dims
    isOpenGrid' = U.fromFunction dims isOpen
    cornerGrid' = 
        B.update (B.make dims Set.empty) $
        withStrategy (parListChunk 1000 rdeepseq) $ 
        map (\(x,y) -> (x,y,Set.fromList $ getInSight isOpen (x,y) corners)) 
            [(x,y) | x <- [0..fst dims-1], y <- [0..snd dims-1]]


{-# INLINE isTileOpen #-}
isTileOpen :: Pathing -> Int -> Int -> Maybe Bool
isTileOpen (Pathing _ g) x y = U.read g (x,y)

{-# INLINE getPath #-}
getPath ::
    Pathing ->
    (Int,Int) ->
    (Int,Int) ->
    Maybe [(Int,Int)]
getPath pathing xy0 xy1 = 
    if inSight (U.readOrElse (isOpenGrid pathing) False) xy0 xy1
    then Just []
    else aStar
        (B.readOrElse (cornerGrid pathing) Set.empty)
        (\(ax,ay) (bx,by) -> sqrt $ fromIntegral $ (ax-bx)^two + (ay-by)^two :: Double)
        (\(x,y) -> fromIntegral $ (x - fst xy1)^two + (y - snd xy1)^two)
        (flip Set.member (B.readOrElse (cornerGrid pathing) Set.empty xy1))
        xy0
    where
    two = 2 :: Int

{-# INLINE getCorners #-}
getCorners :: 
    ((Int,Int) -> Bool) -> -- Node open predicate
    (Int,Int) -> -- Grid width & height
    [(Int,Int)]
getCorners isOpen (width,height) = 
    filter (isCorner isOpen) [(x,y) | x <- [0..width-1], y <- [0..height-1]]

{-# INLINE isCorner #-} 
isCorner :: 
    ((Int,Int) -> Bool) -> -- Node open predicate
    (Int,Int) -> -- Node to check
    Bool
isCorner isOpen (x,y) = 
    cn && (  (not ne && n && e && (nn || ee)) 
          || (not se && s && e && (ss || ee))
          || (not sw && s && w && (ss || ww))
          || (not nw && n && w && (nn || ww))
          )
    where
    cn = isOpen (x, y) -- Checked node is open?
    n  = isOpen (x, (y + 1)) -- Node above (North) is open?
    ne = isOpen ((x + 1), (y + 1)) -- So on and so forth.
    e  = isOpen ((x + 1), y)
    se = isOpen ((x + 1), (y - 1))
    s  = isOpen (x, (y - 1))
    sw = isOpen ((x - 1), (y - 1))
    w  = isOpen ((x - 1), y)
    nw = isOpen ((x - 1), (y + 1))
    nn = isOpen (x, (y + 2))
    ee = isOpen ((x + 2), y)
    ss = isOpen (x, (y - 2))
    ww = isOpen ((x - 2), y)

{-# INLINE getInSight #-}
getInSight :: 
    ((Int,Int) -> Bool) -> -- Node open predicate
    (Int,Int) -> -- Node to check
    [(Int,Int)] -> -- Corners
    [(Int,Int)] -- Corners in sight
getInSight isOpen xy0 = filter (inSight isOpen xy0)

{-# INLINE inSight #-}
inSight :: 
    ((Int,Int) -> Bool) -> -- Node open predicate
    (Int,Int) -> -- Start XY
    (Int,Int) -> -- End XY
    Bool -- End node was visible by start node.
inSight isOpen (x0,y0) (x1,y1) = rat (1 + dx + dy) (x0,y0) err
    where
    dx = abs $ x1 - x0
    dy = abs $ y1 - y0
    err = dx - dy
    x_inc = if x1 > x0 then 1 else -1
    y_inc = if y1 > y0 then 1 else -1
    rat 0 _ _ = True
    rat c (x,y) e =
        if isOpen (x,y) then
            if x == x1 && y == y1 then 
                True 
            else
            if e == 0 then
                (not $ eitherOpen ((x + x_inc),y) (x,(y + y_inc))) ||
                rat (c-1) ((x + x_inc),(y + y_inc)) (e - dy + dx)
            else
                if e > 0 then 
                    rat (c-1) ((x + x_inc),y) (e - dy) 
                else 
                    rat (c-1) (x,(y + y_inc)) (e + dx)
        else
            False
    eitherOpen axy bxy = isOpen axy || isOpen bxy