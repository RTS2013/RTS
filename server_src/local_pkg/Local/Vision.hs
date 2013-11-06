module Local.Vision where

import qualified Local.Matrices.UnboxedMatrix2D as M
import Control.Monad.Primitive (PrimState,PrimMonad)

-- unsafeWith :: (PrimMonad m, V.Unbox a) => Matrix (V.MVector (PrimState m) a) -> (Matrix (V.Vector a) -> b) -> m b'

inSight :: 
    (tile -> Bool) -> -- Is tile "open" predicate
    Matrix tile -> -- Matrix with open/closed nodes
    Int -> -- Start x coordinate
    Int -> -- Start y coordinate
    Int -> -- End x coordinate
    Int -> -- End y coordinate
    [(Int,Int,[(Int,Int)])] -- End node was visible by start node.
inSight isOpen mtrx x0 y0 x1 y1 = rat (1 + dx + dy) x0 y0 err
    where
    dx = abs $ x1 - x0
    dy = abs $ y1 - y0
    err = dx - dy
    x_inc = if x1 > x0 then 1 else -1
    y_inc = if y1 > y0 then 1 else -1
    rat 0 _ _ _ = True
    rat c x y e = case read x y mtrx of
        Just ok -> 
            if isOpen ok then
                if x == x1 && y == y1 then 
                    True 
                else
                if e == 0 then
                    (not $ eitherOpen (x + x_inc) y x (y + y_inc)) ||
                    rat (c-1) (x + x_inc) (y + y_inc) (e - dy + dx)
                else
                    if e > 0 then 
                        rat (c-1) (x + x_inc) y (e - dy) 
                    else 
                        rat (c-1) x (y + y_inc) (e + dx)
            else
                False
        Nothing -> False
    eitherOpen x0 y0 x1 y1 = 
        case read x0 y0 mtrx of
        Just ok -> isOpen ok || 
            case read x1 y1 mtrx of
            Just ok -> isOpen ok
            Nothing -> False
        Nothing -> 
            case read x1 y1 mtrx of
            Just ok -> isOpen ok
            Nothing -> False