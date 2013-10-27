{-# LANGUAGE MultiParamTypeClasses #-}

module Local.Pathing where

import Prelude hiding (read)
import Local.Matrices.Sliced.Matrix2D
import qualified Local.KDT as K
import qualified Data.Vector as V

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

type Matrix tile = V.Vector (V.Vector tile)

inSight :: 
    (tile -> Bool) -> -- Is tile "open" predicate
    Matrix tile -> -- Matrix with open/closed nodes
    Int -> -- Start x coordinate
    Int -> -- Start y coordinate
    Int -> -- End x coordinate
    Int -> -- End y coordinate
    Bool -- End node was visible by start node.
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

setCorners ::
    (tile -> Bool) -> -- Is tile "open" predicate
    Matrix tile -> -- Matrix with potential corners
    ([(Int,Int)] -> tile -> tile) -> -- Assign list of corners to node
    ([(Int,Int)], Matrix tile) -- (List of corners, Matrix with corners setup)
setCorners isOpen mtrx addCorners = 
    (corners, foldl (\m (x,y) -> modify x y 
        (addCorners $ filter 
            (\(x0,y0) -> if x0 == x && y0 == y 
                         then False 
                         else inSight isOpen mtrx x y x0 y0) corners) m) mtrx corners)
  where
    -- Extract all corner nodes from the matrix.
    corners = 
        V.ifoldl' (\ts y xv -> ts ++ 
        V.ifoldl' (\ts x t -> if isCorner x y t then (x,y):ts else ts) [] xv) [] mtrx
      where
        -- Is the node at (x,y) a corner?
        isCorner x y t = 
            let f x y = maybe False isOpen $ read x y mtrx 
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
                ww = f (x - 2) y 
            in
            cn && (
                    (not ne && n && e && (nn || ee)) ||
                    (not se && s && e && (ss || ee)) ||
                    (not sw && s && w && (ss || ww)) ||
                    (not nw && n && w && (nn || ww))
                  )

{-
This function will move a entity through a map while trying to keep it
seperate from other units. It will also make sure the entity doesn't
go through walls.

Here's the general high level steps of this function:

1) Gather nearby units that can push the given entity around.
2) Add up how much the given entity needs to be pushed around by
   using how closely it is hugging other units.
3) Add the units own movement and how it should be pushed around.
4) Use these new coordinates and make sure the entity isn't doing anything naughty.
   This is handled by the movit function which does a buttload of checks to
   ensure the entity isn't going through walls. If a entity does try to go
   through a wall then it is corrected.
-}
{-# INLINE move2D #-}
move2D :: (Eq entity, RealFloat n, Move2D n entity) =>
    n -> -- The largest radius of any and all entities
    (tile -> Bool) -> -- Is tile "open" predicate
    Matrix tile -> -- Matrix of tiles
    K.KDT n entity -> -- The KD-Tree of units
    n -> -- X coordinate you want to move the entity to
    n -> -- Y coordinate you want to move the entity to
    n -> -- How far you want to move the entity
    entity -> -- Entity you want to move
    entity -- Newly positioned entity
move2D maxRadius isOpen matrix kdt x y range entity =
    let ux = getX entity
        uy = getY entity
        ur = getRadius entity -- Entity Radius
        uw = getWeight entity -- Entity Weight
        -- Get all units in range of entity
        isInRange e = (getX e - ux)^2 + (getY e - uy)^2 <= (ur + getRadius e)^2
        nearby = filter (\e -> e /= entity && isInRange e) $ K.inRange kdt [getX,getY] [ux,uy] $ ur + maxRadius
        -- Count units in range
        len = length nearby
        -- Calculate offsets from nearby entities
        (xo,yo) = foldl (\(xo,yo) ou -> 
            let xDif = ux - getX ou
                yDif = uy - getY ou
                -- Semi-arbitrary dampener. Makes sure entity isn't pushed around too much.
                wDif = ((getWeight ou + uw) / fromIntegral len / uw) * 0.8
                -- Distance between units, multiplied by dampener.
                dist = ((ur + getRadius ou) - (sqrt $ xDif^2 + yDif^2)) * wDif
                angl = atan2 yDif xDif 
            in 
            (xo + cos angl * dist, yo + sin angl * dist)) 
            (0,0) nearby 
        -- Newly caluclated X/Y coordinates for the entity
        (nx,ny) = 
            if len < 6 then 
                let angle = atan2 (y - uy) (x - ux) 
                    dist = min (sqrt $ (y - uy)^2 + (x - ux)^2) range
                in 
                    (ux + xo + cos angle * dist, uy + yo + sin angle * dist)
            else 
                (ux + xo, uy + yo)
        -- Current tile X/Y
        (cx,cy) = (floor ux, floor uy) -- Current tile coordinates
        -- Check if a tile in the matrix is open
        f x y = maybe False isOpen $ read x y matrix
        -- Corrects an entity from moving through walls
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
                            setX nx $ setY ny entity
                        else -- Corner closed
                            let tx = fromIntegral $ cx + sx
                                ty = fromIntegral $ cy + sy in
                            if (nx - tx)^2 + (ny - ty)^2 <= ur^2 then
                                -- Get angle from new coords to old coords
                                let na = atan2 (ny - uy) (nx - ux) in
                                    setX (tx `fx'` cos na * ur) $
                                    setY (ty `fy'` sin na * ur) entity
                            else
                                setX nx $ setY ny entity
                    else -- X closed
                        let tx = fromIntegral $ cx + sx in
                        if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                            -- Offset from wall by units radius
                            setX (tx `fx'` ur) $ setY ny entity
                        else
                            setX nx $ setY ny entity
                else -- Y closed
                    if f (cx `ix` 1) cy then -- X open
                        let ty = fromIntegral $ cy + sy in
                        if ny `fy` ur `oy` ty then -- Unit Y is close to wall?
                            -- Offset from wall by units radius
                            setY (ty `fy'` ur) $ setX nx entity
                        else
                            setX nx $ setY ny entity
                    else -- X closed
                        let tx = fromIntegral $ cx + sx
                            ty = fromIntegral $ cy + sy in
                        if ny `fy` ur `oy` ty then -- Unit Y is close to wall?
                            if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                                -- Offset from wall by units radius
                                setX (tx `fx'` ur) $ setY (ty `fy'` ur) entity
                            else
                                -- Offset from wall by units radius
                                setY (ty `fy'` ur) $ setX nx entity
                        else
                            if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                                -- Offset from wall by units radius
                                setX (tx `fx'` ur) $ setY ny entity
                            else
                                setX nx $ setY ny entity
    in setX nx $ setY ny entity
    {-
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
    -}