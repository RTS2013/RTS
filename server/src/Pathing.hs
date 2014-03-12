{-# LANGUAGE Trustworthy #-}

module Pathing where

import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import Control.Concurrent.ParallelIO.Global (parallel_)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar
import Data.Graph.AStar (aStar)

getPath ::
    ((Int,Int) -> Bool) -> -- Is tile "open" predicate
    ((Int,Int) -> V.Vector (Int,Int)) ->
    (Int,Int) -> -- Start XY
    (Int,Int) -> -- Goal XY
    Maybe [(Int,Int)] -- Empty list means you can move directly to goal
getPath isOpen corners xy exy@(gx,gy) = 
    if inSight isOpen xy exy
    then Just [] 
    else aStar 
        (V.foldr' S.insert S.empty . corners)
        (\(ax,ay) (bx,by) -> sqrt $ fromIntegral $ (ax-bx)^two + (ay-by)^two :: Double)
        (\(x,y) -> sqrt $ fromIntegral $ (x-gx)^two + (y-gy)^two)
        (flip S.member endCorners)
        xy
    where 
    two = 2 :: Int
    endCorners = V.foldr' S.insert S.empty $ corners exy

setGroupsM ::
    (Int,Int) -> -- Width/Height
    ((Int,Int) -> IO Bool) -> -- Is open predicate
    ((Int,Int) -> V.Vector (Int,Int) -> IO ()) -> -- Set group at point
    IO ()
setGroupsM (w,h) isOpen setGroup = do
    corners <- getCornersM (w,h) isOpen
    groupsVar <- newTVarIO S.empty
    parallel_ $ flip map [(x,y) | x <- [0..w], y <- [0..h]] $ \xy -> do
        sighted <- V.filterM (inSightM isOpen xy) corners
        group <- atomically $ do
            groups <- readTVar groupsVar
            if S.notMember sighted groups
            then modifyTVar' groupsVar (S.insert sighted) >> return sighted
            else maybe (return sighted) return $ S.lookupLE sighted groups
        setGroup xy group

inSightM :: (Monad m) =>
    ((Int,Int) -> m Bool) -> -- Is tile "open" predicate
    (Int,Int) -> -- Start x/y coordinates
    (Int,Int) -> -- End x/y coordinates
    m Bool -- End node was visible by start node.
inSightM isOpen (x0,y0) (x1,y1) = rat (1 + dx + dy) x0 y0 err
    where
    dx = abs $ x1 - x0
    dy = abs $ y1 - y0
    err = dx - dy
    x_inc = if x1 > x0 then 1 else -1
    y_inc = if y1 > y0 then 1 else -1
    rat 0 _ _ _ = return True
    rat c x y e = do
        aOpen <- isOpen (x,y)
        if aOpen then
            if x == x1 && y == y1 then 
                return True 
            else
            if e == 0 then do
                bOpen <- eitherOpen (x + x_inc) y x (y + y_inc)
                cOpen <- rat (c-1) (x + x_inc) (y + y_inc) (e - dy + dx)
                return $ not bOpen || cOpen
            else
                if e > 0 then 
                    rat (c-1) (x + x_inc) y (e - dy) 
                else 
                    rat (c-1) x (y + y_inc) (e + dx)
        else
            return False
    eitherOpen x0' y0' x1' y1' = do
        a <- isOpen (x0',y0') 
        b <- isOpen (x1',y1')
        return $ a || b

getCornersM :: (Monad m) =>
    (Int,Int) -> -- Width/Height
    ((Int,Int) -> m Bool) -> -- Is tile "open" predicate
    m (V.Vector (Int,Int)) -- List of corners
getCornersM (width,height) isOpen = V.filterM isCorner $ V.fromList [(x,y) | x <- [0..width], y <- [0..height]]
  where
    isCorner (x,y) = do
        cn <- isOpen (x, y) -- Checked node is open?
        n  <- isOpen (x, (y + 1)) -- Node above (North) is open?
        ne <- isOpen ((x + 1), (y + 1)) -- So on and so forth.
        e  <- isOpen ((x + 1), y)
        se <- isOpen ((x + 1), (y - 1))
        s  <- isOpen (x, (y - 1))
        sw <- isOpen ((x - 1), (y - 1))
        w  <- isOpen ((x - 1), y)
        nw <- isOpen ((x - 1), (y + 1))
        nn <- isOpen (x, (y + 2))
        ee <- isOpen ((x + 2), y)
        ss <- isOpen (x, (y - 2))
        ww <- isOpen ((x - 2), y)
        return $
            cn && ( (not ne && n && e && (nn || ee)) 
                  ||(not se && s && e && (ss || ee))
                  ||(not sw && s && w && (ss || ww))
                  ||(not nw && n && w && (nn || ww))
                  )

inSight :: 
    ((Int,Int) -> Bool) -> -- Is tile "open" predicate
    (Int,Int) -> -- Start XY
    (Int,Int) -> -- Goal XY
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

getCorners :: 
    (Int,Int) -> -- Width/Height
    ((Int,Int) -> Bool) -> -- Is tile "open" predicate
    V.Vector (Int,Int) -- List of corners
getCorners (width,height) isOpen = V.filter isCorner $ V.fromList [(x,y) | x <- [0..width], y <- [0..height]]
    where
    isCorner (x,y) = 
        let cn = isOpen (x, y) -- Checked node is open?
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
            ww = isOpen ((x - 2), y) in
        cn && ( (not ne && n && e && (nn || ee)) 
              ||(not se && s && e && (ss || ee))
              ||(not sw && s && w && (ss || ww))
              ||(not nw && n && w && (nn || ww))
              )