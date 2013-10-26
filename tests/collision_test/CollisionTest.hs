{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}

module Main where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import Local.KDT
import Local.Pathing
import qualified Data.Vector as V
import qualified Local.Matrices as M
import qualified Control.Parallel.Strategies as PS
import Local.WindowSize (getWindowSize)

data Unit = Unit { posX, posY, radius, weight :: !Float, reached :: !Bool} deriving (Eq)

instance Move2D Float Unit where
    getWeight = weight
    setWeight w u = u {weight = w}
    getRadius = radius
    setRadius r u = u {radius = r}
    getX = posX
    setX x u = u {posX = x}
    getY = posY
    setY y u = u {posY = y}

data World = World
    { units :: ![Unit]
    , units_kdt :: !(KDT Float Unit)
    , matrix :: V.Vector (V.Vector Bool)
    , mouseX :: Float
    , mouseY :: Float
    }

main :: IO ()
main = do
    (wx,wy) <- getWindowSize
    rs <- fmap (randomRs (0.0::Float, 1.0::Float)) getStdGen
    let numUnits = 8000
        xs = take numUnits rs
        ys = take numUnits $ drop numUnits rs
        units = fmap (\(x,y) -> Unit (x * wx) (y * wy) 1 1 False) $ zip xs ys
        display = FullScreen (floor wx, floor wy)
        background = makeColor8 0 0 0 255
        world = World units (makeKDT [posX,posY] units) (M.make2D 1000 1000 True) 0 0
    playIO display background 10 world (toPicture wx wy) handleEvent stepWorld

handleEvent :: Event -> World -> IO World
handleEvent (EventMotion (x,y)) w = return $ w {mouseX = x, mouseY = y}
handleEvent _ w = return w

toPicture :: Float -> Float -> World -> IO Picture
toPicture wx wy !(World units _ _ _ _) = return 
    $ scale 10 10
    $ pictures $ fmap (\unit -> translate (posX unit - wx/2) (posY unit - wy/2) 
    $ color white 
    $ circle (radius unit)) units

stepWorld :: b -> World -> IO World
stepWorld _ !(w@(World units units_kdt matrix x y)) = return $
    let bumped = PS.parMap PS.rseq (move2D 1 (\_ -> True) matrix units_kdt ((x/10 + 800)) ((y/10 + 600)) 1) units
        kdt = makeKDT [posX,posY] bumped
    in
        World bumped kdt matrix x y