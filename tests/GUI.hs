module GUI where

import Graphics.Gloss
import Codec.BMP
import Local.WindowSize
import Graphics.Gloss.Interface.Pure.Game

data Button a = Button
    { btn_pos :: (Float,Float)
    , btn_dim :: (Float,Float)
    , btn_pic :: Picture
    , btn_fun :: (a -> a)
    } 

makeButton :: BMP -> (a -> a) -> Button a
makeButton bmp = Button (0,0) (fromIntegral x, fromIntegral y) (bitmapOfBMP bmp)
    where (x,y) = bmpDimensions bmp

makeColorButton :: (Float,Float) -> Color -> (a -> a) -> Button a
makeColorButton (w,h) c = Button (0,0) (w,h) (color c $ rectangleSolid w h)

selectButton :: Float -> Float -> [Button a] -> Maybe (a -> a)
selectButton x y btns = 
    case dropWhile (not . p) btns of
        [   ] -> Nothing
        btn:_ -> Just $ btn_fun btn
    where
        p btn = let (bx,by) = btn_pos btn
                    (w,h) = btn_dim btn
                    (hw,hh) = (w/2,h/2) in
                x >= bx - hw && 
                x <= bx + hw && 
                y >= by - hh &&
                y <= by + hh

button2Picture :: Button a -> Picture
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
        -> [Button a] 
        -> ([Button a],(Float,Float))
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

mainy = do
    winDims <- getWindowSize
    let renderButtons s d = pictures . map button2Picture . fst . arrange s d winDims
        buttons = 
            [ makeColorButton (150,150) red (\_ -> "Red")
            , makeColorButton (125,125) blue (\_ -> "Blue")
            , makeColorButton (100,100) yellow (\_ -> "Yellow")
            , makeColorButton (75,75) green (\_ -> "Green")
            ]
    display (FullScreen winDims) black $ pictures
        [ renderButtons West East buttons
        , renderButtons East West $ reverse buttons
        , renderButtons North South buttons
        , renderButtons South North $ reverse buttons
        , renderButtons Northeast South buttons
        , renderButtons Northwest East buttons
        , renderButtons Southeast West buttons
        , renderButtons Southwest North buttons
        ]

main = do 
    winDims <- getWindowSize
    let renderButtons btns = pictures $ map button2Picture btns
        buttons = fst $ arrange Northwest East winDims
            [ makeColorButton (64,64) red (\_ -> "Red")
            , makeColorButton (64,64) blue (\_ -> "Blue")
            , makeColorButton (64,64) yellow (\_ -> "Yellow")
            , makeColorButton (64,64) green (\_ -> "Green")
            ]
    play (FullScreen winDims) black 10 ("Orange",buttons)
        (\(txt,btns) -> pictures [color white $ text txt, renderButtons btns])
        eventHandler
        (\_ world -> world)

eventHandler (EventKey (MouseButton LeftButton) Down _ (x,y)) world@(txt,btns) = 
    maybe world (\f -> (f txt,btns)) $ selectButton x y btns
eventHandler _ world = world