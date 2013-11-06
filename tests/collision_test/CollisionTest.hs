{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}

module Main where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import Local.KDT
import Local.Pathing
import qualified Data.Vector as V
import qualified Local.Matrices.Sliced.Matrix2D as M
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

maxRadius = 2

main :: IO ()
main = do
    (wx,wy) <- getWindowSize
    rs <- fmap (randomRs (0.0::Float, 1.0::Float)) getStdGen
    let numUnits = 4000
        xs = take numUnits rs
        ys = take numUnits $ drop numUnits rs
        units = fmap (\(x,y) -> Unit (x * wx) (y * wy) 0.25 1 False) $ zip xs ys
        display = InWindow "Collision Test" (floor wx, floor wy) (0,0)
        background = makeColor8 0 0 0 255
        world = World units (makeKDT [posX,posY] units) (M.make 1000 1000 True) 0 0
    playIO display background 20 world (toPicture wx wy) handleEvent stepWorld

handleEvent :: Event -> World -> IO World
handleEvent (EventMotion (x,y)) w = return $ w {mouseX = x, mouseY = y}
handleEvent _ w = return w

zoom = 50

toPicture :: Float -> Float -> World -> IO Picture
toPicture wx wy !(World units _ _ _ _) = return 
    $ scale zoom zoom
    -- $ circle 10
    $ pictures $ fmap (\unit -> translate (posX unit - wx/2) (posY unit - wy/2) $ color white $ circleSolid (radius unit)) units

stepWorld :: b -> World -> IO World
stepWorld _ !(w@(World units units_kdt matrix x y)) = return $
    let bumped = PS.parMap PS.rseq (move2D maxRadius (\_ -> True) matrix units_kdt ((x/zoom + 800)) ((y/zoom + 600)) 0.1) units
        kdt = makeKDT [posX,posY] bumped
    in
        World bumped kdt matrix x y