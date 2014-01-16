module Movement 
( Moves(..)
, Point(..)
, Bulk(..)
, inSight
, getCorners
, move2D
) where

import Data.Graph.AStar (aStar)

{- aStar :: (Ord a, Ord c, Num c) =>
         (a -> Set a)     -- ^ The graph we are searching through, given as a function from vertices
                          -- to their neighbours.
         -> (a -> a -> c) -- ^ Distance function between neighbouring vertices of the graph. This will
                          -- never be applied to vertices that are not neighbours, so may be undefined
                          -- on pairs that are not neighbours in the graph.
         -> (a -> c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                          -- distance, or else the path found may not be minimal.
         -> (a -> Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> a             -- ^ The vertex to start searching from.
         -> Maybe [a]     -- ^ An optimal path, if any path exists. This excludes the starting vertex.

-}

data Point = Point 
    { xCoord, yCoord, zCoord :: {-# UNPACK #-} !Float } 
    
data Bulk = Bulk
    { radius, weight :: {-# UNPACK #-} !Float }

class Moves a where
    getMoveIdentity :: a -> Bulk
    getMoveState    :: a -> Point
    setMoveState    :: a -> Point -> a

inSight :: 
    (Int -> Int -> Bool) -> -- Is tile "open" predicate
    Int -> -- Start x coordinate
    Int -> -- Start y coordinate
    Int -> -- End x coordinate
    Int -> -- End y coordinate
    Bool -- End node was visible by start node.
inSight isOpen x0 y0 x1 y1 = rat (1 + dx + dy) x0 y0 err
    where
    dx = abs $ x1 - x0
    dy = abs $ y1 - y0
    err = dx - dy
    x_inc = if x1 > x0 then 1 else -1
    y_inc = if y1 > y0 then 1 else -1
    rat 0 _ _ _ = True
    rat c x y e =
        if isOpen x y then
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
    eitherOpen x0' y0' x1' y1' = isOpen x0' y0' || isOpen x1' y1'

getCorners :: 
    Int -> -- Width of map
    Int -> -- Height of map
    (Int -> Int -> Bool) -> -- Is tile "open" predicate
    [(Int,Int)] -- List of corners
getCorners width height isOpen = filter isCorner [(x,y) | x <- [0..width], y <- [0..height]]
  where
    isCorner (x,y) = 
        let cn = isOpen x y -- Checked node is open?
            n  = isOpen x (y + 1) -- Node above (North) is open?
            ne = isOpen (x + 1) (y + 1) -- So on and so forth.
            e  = isOpen (x + 1) y
            se = isOpen (x + 1) (y - 1)
            s  = isOpen x (y - 1)
            sw = isOpen (x - 1) (y - 1)
            w  = isOpen (x - 1) y
            nw = isOpen (x - 1) (y + 1)
            nn = isOpen x (y + 2)
            ee = isOpen (x + 2) y
            ss = isOpen x (y - 2)
            ww = isOpen (x - 2) y 
        in 
            cn && ( (not ne && n && e && (nn || ee)) 
                  ||(not se && s && e && (ss || ee))
                  ||(not sw && s && w && (ss || ww))
                  ||(not nw && n && w && (nn || ww))
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
move2D :: (Eq entity, Moves entity) =>
    (Int -> Int -> Bool) -> -- Is tile "open" predicate
    (Float -> Float -> Float -> [entity]) -> -- Given an x, y, & radius, returns list of entities
    Float -> -- The largest radius of any and all entities
    Float -> -- X coordinate you want to move the entity to
    Float -> -- Y coordinate you want to move the entity to
    Float -> -- How far you want to move the entity
    entity -> -- Entity you want to move
    entity -- Newly positioned entity
move2D isOpen inRange maxRadius x y range entity = 
    let moveState = getMoveState entity
        moveIdnt  = getMoveIdentity entity
        ux        = xCoord moveState
        uy        = yCoord moveState
        ur        = radius moveIdnt
        uw        = weight moveIdnt
        getX      = xCoord . getMoveState
        getY      = yCoord . getMoveState
        getRadius = radius . getMoveIdentity
        getWeight = weight . getMoveIdentity
        setX vx m  = m { xCoord = vx}
        setY vy m  = m { yCoord = vy}
        -- Get all units in range of entity
        isInRange e = (getX e - ux)^(2 :: Int) + (getY e - uy)^(2 :: Int) <= (ur + getRadius e)^(2 :: Int)
        nearby = filter (\e -> e /= entity && isInRange e) $ inRange ux uy $ ur + maxRadius
        -- Count units in range
        len = length nearby
        -- Calculate offsets from nearby entities
        (xo,yo) = foldl (\(xo',yo') ou -> 
            let xDif = ux - getX ou
                yDif = uy - getY ou
                -- Semi-arbitrary dampener. Makes sure entity isn't pushed around too much.
                wDif = ((getWeight ou + uw) / fromIntegral len / uw) * 0.8
                -- Distance between units, multiplied by dampener.
                dist = ((ur + getRadius ou) - (sqrt $ xDif^(2 :: Int) + yDif^(2 :: Int))) * wDif
                angl = atan2 yDif xDif 
            in 
            (xo' + cos angl * dist, yo' + sin angl * dist)) 
            (0,0) nearby 
        -- Newly caluclated X/Y coordinates for the entity
        (nx,ny) = 
            if len < 6 then 
                let angle = atan2 (y - uy) (x - ux) 
                    dist = min (sqrt $ (y - uy)^(2 :: Int) + (x - ux)^(2 :: Int)) range
                in 
                    (ux + xo + cos angle * dist, uy + yo + sin angle * dist)
            else 
                (ux + xo, uy + yo)
        -- Current tile X/Y
        (cx,cy) = (floor ux, floor uy) -- Current tile coordinates
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
              = if isOpen cx (cy `iy` 1) then -- Y open
                    if isOpen (cx `ix` 1) cy then -- X open
                        if isOpen (cx `ix` 1) (cy `iy` 1) then -- Corner open
                            setX nx $ setY ny moveState
                        else -- Corner closed
                            let tx = fromIntegral $ cx + sx
                                ty = fromIntegral $ cy + sy in
                            if (nx - tx)^(2 :: Int) + (ny - ty)^(2 :: Int) <= ur^(2 :: Int) then
                                -- Get angle from new coords to old coords
                                let na = atan2 (ny - uy) (nx - ux) in
                                    setX (tx `fx'` cos na * ur) $
                                    setY (ty `fy'` sin na * ur) moveState
                            else
                                setX nx $ setY ny moveState
                    else -- X closed
                        let tx = fromIntegral $ cx + sx in
                        if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                            -- Offset from wall by units radius
                            setX (tx `fx'` ur) $ setY ny moveState
                        else
                            setX nx $ setY ny moveState
                else -- Y closed
                    if isOpen (cx `ix` 1) cy then -- X open
                        let ty = fromIntegral $ cy + sy in
                        if ny `fy` ur `oy` ty then -- Unit Y is close to wall?
                            -- Offset from wall by units radius
                            setY (ty `fy'` ur) $ setX nx moveState
                        else
                            setX nx $ setY ny moveState
                    else -- X closed
                        let tx = fromIntegral $ cx + sx
                            ty = fromIntegral $ cy + sy in
                        if ny `fy` ur `oy` ty then -- Unit Y is close to wall?
                            if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                                -- Offset from wall by units radius
                                setX (tx `fx'` ur) $ setY (ty `fy'` ur) moveState
                            else
                                -- Offset from wall by units radius
                                setY (ty `fy'` ur) $ setX nx moveState
                        else
                            if nx `fx` ur `ox` tx then -- Unit X is close to wall?
                                -- Offset from wall by units radius
                                setX (tx `fx'` ur) $ setY ny moveState
                            else
                                setX nx $ setY ny moveState
    in setMoveState entity $ 
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