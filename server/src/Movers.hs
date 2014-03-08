module Movers where

type X = Float
type Y = Float
type Radius = Float
type Weight = Float
type Distance = Float

moveM ::
    ((X,Y) -> Radius -> [(X,Y,Radius,Weight)]) ->
    Radius -> -- Max radius of any entity
    (X,Y) -> -- Desination
    Distance -> -- Distance to move
    (X,Y,Radius,Weight) -> -- Entity being moved
    (X,Y) -- New coordinates for entity
moveM getInRange maxR (dx,dy) dist (sx,sy,r,w) = newXY $ push inRange
    where
    two = 2 :: Int
    len = length inRange
    inRange = filter (\(nx,ny,nr,_) -> 
            nx /= sx && ny /= sy && ((nx-sx)^two + (ny-sx)^two <= (r+nr)^two)
        ) $ getInRange (sx,sy) (maxR + r)
    push xs = foldr 
        (\(nx,ny) (ox,oy) -> (ox+nx,oy+ny)) (0,0) $ map (\(nx,ny,nr,nw) -> 
            let (xDif,yDif) = (sx-nx,sy-ny) in
            let wDif = (nw + w) / fromIntegral len / w * 0.8 in
            let rDif = (r + nr) - (sqrt $ xDif^two + yDif^two) in
            let angl = atan2 yDif xDif in
            (cos angl * rDif * wDif, sin angl * rDif * wDif)
        ) xs
    newXY (ox,oy) = 
        if len < 6 then 
            let angl = atan2 (dy - sy) (dx - sx) 
                rDif = min (sqrt $ (dy - sy)^two + (dx - sx)^two) dist
            in 
                (sx + ox + cos angl * rDif, sy + oy + sin angl * rDif)
        else 
            (sx + ox, sy + oy)

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
    (fx,fy) -- Float X/Y addition/subtraction (should be same as ix & iy)
    (fx',fy') -- Float X addition/subtraction (should be opposite fx & fy)
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
                return ((bx, by) :: (Float,Float))
            else -- Corner closed
                let tx = fromIntegral $ (floor $ ax + px :: Int)
                    ty = fromIntegral $ (floor $ ay + py :: Int) in
                if (bx - tx)^two + (by - ty)^two <= r^two then
                    -- Get angle from new coords to old coords
                    let na = atan2 (by - ay) (bx - ax) :: Float in
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