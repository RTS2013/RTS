module GUI 
( makeButton
, makeColorButton
, button2Picture
, Direction(..)
, Button()
, selectButton
, flipDirection
, arrange
) where 

import Graphics.Gloss
import Codec.BMP
import Local.WindowSize
import Graphics.Gloss.Interface.Pure.Game

data Button = Button
    { btn_pos :: (Float,Float)
    , btn_dim :: (Float,Float)
    , btn_pic :: Picture
    , btn_name :: String
    } deriving (Eq,Show)

makeButton :: BMP -> String -> Button
makeButton bmp = Button (0,0) (fromIntegral x, fromIntegral y) (bitmapOfBMP bmp)
    where (x,y) = bmpDimensions bmp

makeColorButton :: (Float,Float) -> Color -> String -> Button
makeColorButton (w,h) c = Button (0,0) (w,h) (color c $ rectangleSolid w h)

selectButton :: Float -> Float -> [Button] -> Maybe String
selectButton x y btns = 
    case dropWhile (not . p) btns of
        [   ] -> Nothing
        btn:_ -> Just $ btn_name btn
    where
        p btn = let (bx,by) = btn_pos btn
                    (w,h) = btn_dim btn
                    (hw,hh) = (w/2,h/2) in
                x >= bx - hw && 
                x <= bx + hw && 
                y >= by - hh &&
                y <= by + hh

button2Picture :: Button -> Picture
button2Picture btn = translate x y $ btn_pic btn
    where
    (x,y) = btn_pos btn

data Direction
    = North
    | South
    | East
    | West
    | Northeast
    | Northwest
    | Southeast
    | Southwest

flipDirection :: Direction -> Direction 
flipDirection North = South 
flipDirection South = North 
flipDirection East = West 
flipDirection West = East 
flipDirection Northeast = Southwest 
flipDirection Northwest = Southeast 
flipDirection Southeast = Northwest 
flipDirection Southwest = Northeast 

arrange :: Direction -- Start location 
        -> Direction -- Align direction 
        -> (Int,Int) -- Window size 
        -> [Button] 
        -> ([Button],(Float,Float))
arrange start direction (winWidth,winHeight) btns = 
    let (pics,dims) = 
            foldl (\(pics,(xAcc,yAcc)) btn -> 
                let (bw,bh) = btn_dim btn
                    (halfW,halfH) = (bw / 2, bh / 2) 
                    pic = btn {btn_pos = ((xStart + xAcc) `fx` halfW,(yStart + yAcc) `fy` halfH) }
                in
                (pic : pics, (xAcc `xf` bw, yAcc `yf` bh))) ([],(0,0)) btns 
    in
        (pics, dims)
    where
        xStart = xMorph start 0 xOffset
        yStart = yMorph start 0 yOffset
        xf = xMorph direction
        yf = yMorph direction
        fx = xMorph $ flipDirection start
        fy = yMorph $ flipDirection start
        xOffset = fromIntegral winWidth / 2
        yOffset = fromIntegral winHeight / 2
        xMorph East = (+)
        xMorph West = (-)
        xMorph Northeast = (+)
        xMorph Northwest = (-)
        xMorph Southeast = (+)
        xMorph Southwest = (-)
        xMorph _ = flip seq
        yMorph North = (+)
        yMorph South = (-)
        yMorph Northeast = (+)
        yMorph Northwest = (+)
        yMorph Southeast = (-)
        yMorph Southwest = (-)
        yMorph _ = flip seq