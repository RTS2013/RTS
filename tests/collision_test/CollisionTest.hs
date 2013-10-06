{-# LANGUAGE BangPatterns #-}

module Main where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import Local.KDT
import Data
import Collision

main :: IO ()
main = do
    rs <- fmap (randomRs (0.0::Float,1.0::Float)) getStdGen
    let numUnits = 2500
        xs = take numUnits rs
        ys = take numUnits $ drop numUnits rs
        units = fmap (\(x,y) -> Unit (x * 4000 - 2000) (y * 4000 - 2000) 10 1 False) $ zip xs ys
        display = InWindow "Collisions" (800,600) (32,32)
        background = makeColor8 0 0 0 255
        world = World units (makeKDT [posX,posY] units)

    playIO display background 20 world toPicture (\_ w -> return w) stepWorld

toPicture :: World -> IO Picture
toPicture !(World units _) = return 
    $ pictures $ fmap (\unit -> translate (posX unit - 400) (posY unit - 300) 
    $ color white 
    $ circle (radius unit)) units

stepWorld :: b -> World -> IO World
stepWorld _ !(w@(World units units_kdt)) = return $
    let bumped = move units_kdt units
        --moved = fmap moveToCenter bumped
        kdt = makeKDT [posX,posY] bumped
    in
        World bumped kdt

moveToCenter :: Unit -> Unit
moveToCenter !u =
    let x = posX u
        y = posY u
        a = atan2 (300-y) (400-x)
        nx = cos a * 3 + x
        ny = sin a * 3 + y
    in
        u {posX = nx, posY = ny}