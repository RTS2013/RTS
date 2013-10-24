{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}

module Main where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import Local.KDT
import Local.Pathing
import qualified Data.Vector as V
import qualified Local.Matrices as M

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
    }

main :: IO ()
main = do
    rs <- fmap (randomRs (0.0::Float, 1.0::Float)) getStdGen
    let numUnits = 5000
        xs = take numUnits rs
        ys = take numUnits $ drop numUnits rs
        units = fmap (\(x,y) -> Unit (x * 800) (y * 600) 1 1 False) $ zip xs ys
        display = InWindow "Collisions" (800,600) (32,32)
        background = makeColor8 0 0 0 255
        world = World units (makeKDT [posX,posY] units) $ M.make2D 1000 1000 True
    playIO display background 10 world toPicture (\_ w -> return w) stepWorld

toPicture :: World -> IO Picture
toPicture !(World units _ _) = return 
    $ scale 20 20
    $ pictures $ fmap (\unit -> translate (posX unit - 400) (posY unit - 300) 
    $ color white 
    $ circle (radius unit)) units

stepWorld :: b -> World -> IO World
stepWorld _ !(w@(World units units_kdt matrix)) = return $
    let bumped = map (move2D 1 (\_ -> True) matrix units_kdt 400 300 0.5) units
        kdt = makeKDT [posX,posY] bumped
    in
        World bumped kdt matrix

moveToCenter :: Unit -> Unit
moveToCenter !u =
    let x = posX u
        y = posY u
        a = atan2 (300-y) (400-x)
        nx = cos a * 3 + x
        ny = sin a * 3 + y
    in
        u {posX = nx, posY = ny}