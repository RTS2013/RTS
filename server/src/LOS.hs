module LOS
( addVision
, subtractVision
, ring
, fieldOfVision
, fieldOfVisionM
) where

import Data.List (nub)
import RIO.Prelude
import Control.Monad (foldM)
import qualified RIO.Grid as G

-- Returns list of newly discoevered tiles after adding vision
addVision :: G.MGrid Int -> (Int -> Int -> RIO ReadWrite Bool) -> (Int,Int) -> [(Int,Int)] -> RIO ReadWrite [(Int,Int)]
addVision grid isOpen start xs = do
	fov <- fieldOfVisionM isOpen start xs
	foldM (\discovered xy -> do
		      mVal <- G.read xy grid
		      case mVal of
		          Nothing -> return discovered
		          Just val -> do
				      if val < 0
				      then G.write xy 1 grid >> return (xy:discovered)
				      else G.write xy (val + 1) grid >> return discovered
		  ) [] fov


subtractVision :: G.MGrid Int -> (Int -> Int -> RIO ReadWrite Bool) -> (Int,Int) -> [(Int,Int)] -> RIO ReadWrite ()
subtractVision grid isOpen start xs = fieldOfVisionM isOpen start xs >>= mapM_ (\xy -> G.modify xy (\a -> a - 1) grid)


ring :: Int -> [(Int,Int)]
ring r' = nub $ map (\n -> ( floor $ cos (n * slice) * r
	                       , floor $ sin (n * slice) * r
	                       )
                    ) 
                    [0..fromIntegral rays - 1]
	where
	r = fromIntegral r' :: Double
	cir = 2 * pi * r
	rays :: Int
	rays = (floor cir - (floor cir `mod` 4)) * 128
	slice = (2 * pi) / fromIntegral rays


inSight :: 
    (Int -> Int -> Bool) -> -- Is tile "open" predicate
    (Int,Int) -> -- Start coordinates
    (Int,Int) -> -- End coordinates
    [(Int,Int)] -- Visible nodes on ray
inSight isOpen start@(x0,y0) (x1,y1) = start : rat (1 + dx + dy) x0 y0 err
    where
    dx = abs $ x1 - x0
    dy = abs $ y1 - y0
    err = dx - dy
    x_inc = if x1 > x0 then 1 else -1
    y_inc = if y1 > y0 then 1 else -1
    rat 0 _ _ _ = []
    rat c x y e =
        if isOpen x y then
            if x == x1 && y == y1 then 
                [(x,y)]
            else
            if e == 0 && eitherOpen (x + x_inc) y x (y + y_inc) then
                (x,y) : rat (c - 1) (x + x_inc) (y + y_inc) (e - dy + dx)
            else
                if e > 0 then 
                    (x,y) : rat (c - 1) (x + x_inc) y (e - dy) 
                else 
                    (x,y) : rat (c - 1) x (y + y_inc) (e + dx)
        else
            []
    eitherOpen x0' y0' x1' y1' = isOpen x0' y0' || isOpen x1' y1'


fieldOfVision :: (Int -> Int -> Bool) -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
fieldOfVision isOpen start = nub . concat . map (inSight isOpen start)


inSightM :: (Functor m, Monad m) =>
    (Int -> Int -> m Bool) -> -- Is tile "open" predicate
    (Int,Int) -> -- Start coordinates
    (Int,Int) -> -- End coordinates
    m ([(Int,Int)]) -- Visible nodes on ray
inSightM isOpen start@(x0,y0) (x1,y1) = fmap (start:) $ rat (1 + dx + dy) x0 y0 err
    where
    dx = abs $ x1 - x0
    dy = abs $ y1 - y0
    err = dx - dy
    x_inc = if x1 > x0 then 1 else -1
    y_inc = if y1 > y0 then 1 else -1
    rat 0 _ _ _ = return []
    rat c x y e =
        ifM (isOpen x y) 
        thenM (
            if x == x1 && y == y1 
            then return [(x,y)]
            else eitherOpen (x + x_inc) y x (y + y_inc) >>= \p -> 
	             if e == 0 && p 
	             then fmap ((x,y):) $ rat (c - 1) (x + x_inc) (y + y_inc) (e - dy + dx)
	             else if e > 0 
	                  then fmap ((x,y):) $ rat (c - 1) (x + x_inc) y (e - dy) 
	                  else fmap ((x,y):) $ rat (c - 1) x (y + y_inc) (e + dx)
        )
        elseM (
            return []
        )
    eitherOpen x0' y0' x1' y1' = do
    	a <- isOpen x0' y0'
    	b <- isOpen x1' y1'
    	return $ a || b


fieldOfVisionM :: (Functor m, Monad m) => (Int -> Int -> m Bool) -> (Int,Int) -> [(Int,Int)] -> m [(Int,Int)]
fieldOfVisionM isOpen start = fmap (nub . concat) . sequence . map (inSightM isOpen start)


ifM :: (Monad m) => m Bool -> ThenM -> m a -> ElseM -> m a -> m a
ifM p _ a _ b = do
	result <- p
	if result then a else b

thenM = ThenM
elseM = ElseM
data ThenM = ThenM
data ElseM = ElseM