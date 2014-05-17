module Movers where

import Data.List (foldl')

type X = Double
type Y = Double
type Radius = Double
type Weight = Double
type Distance = Double

{-# INLINE move #-}
move ::
    (X,Y,Radius,Weight) -> -- Entity being moved
    (X,Y) -> -- Desination
    Distance -> -- Distance to move
    [(X,Y,Radius,Weight)] -> -- Entities in range
    (X,Y) -- New coordinates for entity
move (sx,sy,r,w) (dx,dy) dist nearby = newXY push
    where
    two = 2 :: Int
    len = length inRange
    inRange = flip filter nearby $ \(nx,ny,_,_) -> nx /= sx && ny /= sy
    push = foldl' 
        (\(nx,ny) (ox,oy) -> (nx+ox,ny+oy)) (0,0) $ map (\(nx,ny,nr,nw) -> 
            let (xDif,yDif) = (sx-nx,sy-ny) in
            let wDif = (w + nw) / (2 * w) / (fromIntegral len + 1) in
            let rDif = (r + nr) - (sqrt $ (xDif^two) + (yDif^two)) in
            let angl = atan2 yDif xDif in
            (cos angl * rDif * wDif, sin angl * rDif * wDif)
        ) inRange
    newXY (ox,oy) = 
        if len < 6 then 
            let angl = atan2 (dy - sy) (dx - sx)
                rDif = min (sqrt $ (dy - sy)^two + (dx - sx)^two) dist
            in 
                (sx + ox + cos angl * rDif, sy + oy + sin angl * rDif)
        else 
            (sx + ox, sy + oy)

{-# INLINE wallStop #-}
wallStop :: (Monad m) => 
    ((X,Y) -> m Bool) -> 
    (X,Y,Radius) -> 
    (X,Y) -> 
    m (X,Y)
wallStop isOpen (ax,ay,r) (bx,by) = 
    if by >= ay then
        if bx >= ax then 
            -- NE move
            angleWallStop isOpen (1,1) ((+),(+)) ((-),(-)) ((>),(>)) (ax,ay,r) (bx,by)
        else
            -- NW move
            angleWallStop isOpen (0,1) ((-),(+)) ((+),(-)) ((<),(>)) (ax,ay,r) (bx,by)
    else
        if bx >= ax then 
            -- SE move
            angleWallStop isOpen (1,0) ((+),(-)) ((-),(+)) ((>),(<)) (ax,ay,r) (bx,by)
        else 
            -- SW move
            angleWallStop isOpen (0,0) ((-),(-)) ((+),(+)) ((<),(<)) (ax,ay,r) (bx,by)

{-# INLINE angleWallStop #-}
angleWallStop :: (Monad m) =>
    ((X,Y) -> m Bool) -> 
    (X,Y) ->
    (X -> X -> X, Y -> Y -> Y) ->
    (X -> X -> X, Y -> Y -> Y) ->
    (X -> X -> Bool, Y -> Y -> Bool) ->
    (X,Y,Radius) ->
    (X,Y) ->
    m (X,Y)
angleWallStop 
    isOpen
    (px,py) -- Shift check X/Y coord (1 or 0)
    (fx,fy) -- Double X/Y addition/subtraction (should be same as ix & iy)
    (fx',fy') -- Double X addition/subtraction (should be opposite fx & fy)
    (ox,oy) -- Greater when ix/iy is (+). Less when ix/iy is (-).
    (ax,ay,r) -- Start X/Y
    (bx,by) -- New X/Y
    = do
    let two = 2 :: Int
    yOpen <- isOpen (ax, (ay `fy` 1))
    if yOpen then do -- Y open
        xOpen <- isOpen ((ax `fx` 1), ay)
        if xOpen then do -- X open
            xyOpen <- isOpen ((ax `fx` 1), (ay `fy` 1))
            if xyOpen then -- Corner open
                return ((bx, by) :: (Double,Double))
            else -- Corner closed
                let tx = fromIntegral $ (floor $ ax + px :: Int)
                    ty = fromIntegral $ (floor $ ay + py :: Int) in
                if (bx - tx)^two + (by - ty)^two <= r^two then
                    -- Get angle from new coords to old coords
                    let na = atan2 (by - ay) (bx - ax) :: Double in
                    return (tx `fx` (cos na * min 0.5 r), ty `fy` (sin na * min 0.5 r))
                else
                    return (bx, by)
        else
            let tx = fromIntegral $ (floor $ ax + px :: Int) in
            if bx `fx` r `ox` tx then -- Unit X is close to wall?
                -- Offset from wall by units radius
                return (tx `fx'` r, by)
            else
                return (bx, by)
    else do -- Y closed
        xOpen <- isOpen ((ax `fx` 1), ay)
        if xOpen then -- X open
            let ty = fromIntegral $ (floor $ ay + py :: Int) in
            if by `fy` r `oy` ty then -- Unit Y is close to wall?
                -- Offset from wall by units radius
                return (bx,ty `fy'` r)
            else
                return (bx,by)
        else -- X closed
            let tx = fromIntegral $ (floor $ ax + px :: Int)
                ty = fromIntegral $ (floor $ ay + py :: Int) in
            if by `fy` r `oy` ty then -- Unit Y is close to wall?
                if bx `fx` r `ox` tx then -- Unit X is close to wall?
                    -- Offset from wall by units radius
                    return (tx `fx'` r, ty `fy'` r)
                else
                    -- Offset from wall by units radius
                    return (bx, ty `fy'` r)
            else
                if bx `fx` r `ox` tx then -- Unit X is close to wall?
                    -- Offset from wall by units radius
                    return (tx `fx'` r, by)
                else
                    return (bx, by)