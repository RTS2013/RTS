{-# LANGUAGE MultiParamTypeClasses #-}

module Local.Pathing where

import Local.Matrices
import qualified Local.KDT as K
import qualified Data.Vector as V

-- Class of data that is possibly pathable
class Path a where
    isOpen :: a -> Bool

-- Class of data that can utilize the move function.
class Move2D n a where
    getWeight :: a -> n
    setWeight :: n -> a -> a
    getRadius :: a -> n
    setRadius :: n -> a -> a
    getX :: a -> n
    setX :: n -> a -> a
    getY :: a -> n
    setY :: n -> a -> a

-- Class of data that moves in a 3D environment.
class (Move2D a n) => Move3D a n where
    getZ :: a -> n
    setZ :: n -> a -> n

type Matrix a = V.Vector (V.Vector a)

inSight :: (Path a) =>
    Matrix a -> -- Matrix with open/closed nodes
    Int -> -- Start x coordinate
    Int -> -- Start y coordinate
    Int -> -- End x coordinate
    Int -> -- End y coordinate
    Bool -- End node was visible by start node.
inSight mtrx x0 y0 x1 y1 = rat (1 + dx + dy) x0 y0 err
    where
    dx = abs $ x1 - x0
    dy = abs $ y1 - y0
    err = dx - dy
    x_inc = if x1 > x0 then 1 else -1
    y_inc = if y1 > y0 then 1 else -1
    rat 0 _ _ _ = True
    rat c x y e = case get2D x y mtrx of
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
    eitherOpen x0 y0 x1 y1 = case get2D x0 y0 mtrx of
        Just ok -> isOpen ok || case get2D x1 y1 mtrx of
            Just ok -> isOpen ok
            Nothing -> False
        Nothing -> case get2D x1 y1 mtrx of
            Just ok -> isOpen ok
            Nothing -> False

setCorners :: (Path a) =>
    Matrix a -> -- Matrix with potential corners
    ([(Int,Int)] -> a -> a) -> -- Assign list of corners to node
    ([(Int,Int)], Matrix a) -- (List of corners, Matrix with corners setup)
setCorners mtrx addCorners = 
    (corners, foldl (\m (x,y) -> update2D x y 
        (addCorners $ filter 
            (\(x0,y0) -> if x0 == x && y0 == y 
                         then False 
                         else inSight mtrx x y x0 y0) corners) m) mtrx corners)
  where
    -- Extract all corner nodes from the matrix.
    corners = 
        V.ifoldl' (\ts y xv -> ts ++ 
        V.ifoldl' (\ts x t -> if isCorner x y t then (x,y):ts else ts) [] xv) [] mtrx
      where
        -- Is the node at (x,y) a corner?
        isCorner x y t = 
            let f x y = maybe False isOpen $ get2D x y mtrx 
                cn = isOpen t -- Checked node is open?
                n  = f x (y + 1) -- Node above (North) is open?
                ne = f (x + 1) (y + 1) -- So on and so forth.
                e  = f (x + 1) y
                se = f (x + 1) (y - 1)
                s  = f x (y - 1)
                sw = f (x - 1) (y - 1)
                w  = f (x - 1) y
                nw = f (x - 1) (y + 1)
                nn = f x (y + 2)
                ee = f (x + 2) y
                ss = f x (y - 2)
                ww = f (x - 2) y in
            cn && (
                (not ne && n && e && (nn || ee)) ||
                (not se && s && e && (ss || ee)) ||
                (not sw && s && w && (ss || ww)) ||
                (not nw && n && w && (nn || ww)))

{-
This function will move a unit through a map while trying to keep it
seperate from other units. It will also make sure the unit doesn't
go through walls.

Here's the general high level steps of this function:

1) Gather nearby units that can push the given unit around.
2) Add up how much the given unit needs to be pushed around by
   using how closely it is hugging other units.
3) Add the units own movement and how it should be pushed around.
4) Use these new coordinates and make sure the unit isn't doing anything naughty.
   This is handled by the movit function which does a buttload of checks to
   ensure the unit isn't going through walls. If a unit does try to go
   through a wall then it is corrected.
-}
move2D :: (Eq a, RealFloat n, Move2D n a, Path t) =>
    Matrix t -> -- Matrix of tiles
    K.KDT n a -> -- The KD-Tree of units
    n -> -- X coordinate you want to move the unit to
    n -> -- Y coordinate you want to move the unit to
    n -> -- How far you want to move the unit
    a -> -- Unit you want to move
    a -- Newly positioned unit
move2D matrix kdt x y range u = 
    let ux = getX u
        uy = getY u
        ur = getRadius u -- Unit Radius
        uw = getWeight u -- Unit Weight
        -- Get all units in range of u
        inRng = filter (/=u) $ K.inRange kdt [getX,getY] [ux,uy] (ur*2) 
        -- Count units in range
        inLen = fromIntegral $ length inRng
        -- Calculate offsets from nearby units
        (xo,yo) = foldl (\(xo,yo) ou -> 
            let xDif = ux - getX ou
                yDif = uy - getY ou
                -- Semi-arbitrary dampener. Makes sure unit isn't pushed around too much.
                wDif = ((getWeight ou + uw) / inLen / uw) * 0.8
                -- Distance between units, multiplied by dampener.
                dist = ((ur + getRadius ou) - (sqrt $ xDif^2 + yDif^2)) * wDif
                angl = atan2 yDif xDif in 
            (xo + cos angl * dist, yo + sin angl * dist)) 
            (0,0) inRng 
        angle = atan2 (y - uy) (x - ux)
        -- Newly caluclated X/Y coordinates for the unit
        nx = ux + xo + cos angle * range
        ny = uy + yo + sin angle * range
        -- Current tile X/Y
        (cx,cy) = (floor ux, floor uy) -- Current tile coordinates
        -- Check if a tile in the matrix is open
        f x y = maybe False isOpen $ get2D x y matrix
        movit sx -- Shift check X coord (1 or 0)
              sy -- Shift check Y coord (1 or 0)
              ix -- Int X addition/subtraction
              iy -- Int Y addition/subtraction
              fx -- Float X addition/subtraction (should be same as ix)
              fy -- Float Y addition/subtraction (should be same as iy)
              fx' -- Float X addition/subtraction (should be opposite fx)
              fy' -- Float Y addition/subtraction (should be opposite fy)
              ox -- Greater when ix is (+). Less when ix is (-).
              oy -- Greater when iy is (+). Less when iy is (-).
              = if f cx (cy `iy` 1) then -- Y open
                    if f (cx `ix` 1) cy then -- X open
                        if f (cx `ix` 1) (cy `iy` 1) then -- Corner open
                            setX nx $ setY ny u
                        else -- Corner closed
                            let tx = fromIntegral $ cx + sx
                                ty = fromIntegral $ cy + sy in
                            if (nx - tx)^2 + (ny - ty)^2 <= ur^2 then
                                -- Get angle from new coords to old coords
                                let na = atan2 (ny - uy) (nx - ux) in
                                    setX (tx `fx'` cos na * ur) $
                                    setY (ty `fy'` sin na * ur) u
                            else
                                setX nx $ setY ny u
                    else -- X closed
                        let tx = fromIntegral $ cx + sx in
                        if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                            -- Offset from wall by units radius
                            setX (tx `fx'` ur) $ setY ny u
                        else
                            setX nx $ setY ny u
                else -- Y closed
                    if f (cx `ix` 1) cy then -- X open
                        let ty = fromIntegral $ cy + sy in
                        if ny `fy` ur `oy` ty then -- Unit Y is close to wall?
                            -- Offset from wall by units radius
                            setY (ty `fy'` ur) $ setX nx u
                        else
                            setX nx $ setY ny u
                    else -- X closed
                        let tx = fromIntegral $ cx + sx
                            ty = fromIntegral $ cy + sy in
                        if ny `fy` ur `oy` ty then -- Unit Y is close to wall?
                            if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                                -- Offset from wall by units radius
                                setX (tx `fx'` ur) $ setY (ty `fy'` ur) u
                            else
                                -- Offset from wall by units radius
                                setY (ty `fy'` ur) $ setX nx u
                        else
                            if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                                -- Offset from wall by units radius
                                setX (tx `fx'` ur) $ setY ny u
                            else
                                setX nx $ setY ny u
    in
        if ny >= uy then
            if nx >= ux then 
                -- NE move
                movit 1 1 (+) (+) (+) (+) (-) (-) (>) (>)
            else 
                -- NW move
                movit 0 1 (-) (+) (-) (+) (+) (-) (<) (>)
        else
            if nx >= ux then 
                -- SE move
                movit 1 0 (+) (-) (+) (-) (-) (+) (>) (<)
            else 
                -- SW move
                movit 0 0 (-) (-) (-) (-) (+) (+) (<) (<)