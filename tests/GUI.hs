module GUI where

import Graphics.Gloss
import Codec.BMP
import Local.WindowSize

data Button a = Button
    { btn_dim :: (Int,Int)
    , btn_pic :: Picture
    , btn_fun :: (a -> a)
    }

makeButton :: BMP -> (a -> a) -> Button a
makeButton bmp = Button (bmpDimensions bmp) (bitmapOfBMP bmp)

makeColorButton :: (Int,Int) -> Color -> (a -> a) -> Button a
makeColorButton (w,h) c = Button (w,h) (color c $ rectangleSolid (fromIntegral w) (fromIntegral h))

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
        -> [Button a] 
        -> (Picture,(Float,Float))
arrange start direction (winWidth,winHeight) btns = 
    let (pics,dims) = 
            foldl (\(pics,(xAcc,yAcc)) btn -> 
                let (bw,bh) = btn_dim btn
                    (halfW,halfH) = (fromIntegral bw / 2, fromIntegral bh / 2) 
                    pic = translate ((xStart + xAcc) `fx` halfW) 
                                    ((yStart + yAcc) `fy` halfH) 
                                    (btn_pic btn)
                in
                (pic : pics, (xAcc `xf` fromIntegral bw, yAcc `yf` fromIntegral bh))) ([],(0,0)) btns 
    in
        (pictures pics, dims)
    where
    xStart = xMorph start 0 xOffset :: Float
    yStart = yMorph start 0 yOffset :: Float
    xf = xMorph direction
    yf = yMorph direction
    fx = xMorph $ flipDirection start
    fy = yMorph $ flipDirection start
    xOffset = fromIntegral winWidth / 2 :: Float
    yOffset = fromIntegral winHeight / 2 :: Float
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

main = do
    winDims <- getWindowSize
    let buttons = [ makeColorButton (150,150) red id
                  , makeColorButton (125,125) blue id
                  , makeColorButton (100,100) yellow id
                  , makeColorButton (75,75) green id
                  ]
    display (FullScreen winDims) black $ pictures
        [ fst $ arrange West East winDims buttons
        , fst $ arrange East West winDims $ reverse buttons
        , fst $ arrange North South winDims buttons
        , fst $ arrange South North winDims $ reverse buttons
        , fst $ arrange Northeast South winDims buttons
        , fst $ arrange Northwest East winDims buttons
        , fst $ arrange Southeast West winDims buttons
        , fst $ arrange Southwest North winDims buttons
        ]
-- $ translate (-800 + 10) (-600 + 10) $ color white $ rectangleSolid 20 20
-- 