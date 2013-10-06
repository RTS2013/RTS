{-# LANGUAGE BangPatterns #-}

module Collision where
    
-- dist = range * cos radianZ | range * (adjacent / hypotenuse)
-- xo = dist * cos radianXY | dist * (adjacent / hypotenuse)
-- yo = dist * sin radianXY | dist * (opposite / hypotenuse)
-- zo = dist * tan radianXY

import Control.Parallel.Strategies
import qualified Data.Vector as V
import qualified Local.Matrices as M
import qualified Local.KDT as K
import qualified Data as D

type Matrix a = V.Vector (V.Vector a)

move :: {-Matrix Bool ->-} K.KDT Float D.Unit -> [D.Unit] -> [D.Unit]
move {-tiles-} !kdt = parMap rdeepseq $ \unit ->
    let ux = D.posX unit
        uy = D.posY unit
        ur = D.radius unit
        inRng = filter (/=unit) $ K.inRange kdt [D.posX,D.posY] [ux,uy] (ur*2) 
        rngCount = fromIntegral $ length inRng
    in 
    -- Spread out units from one another based on how close they are to each other.
    -- Makes sense right? NO! Doesn't work as well as hoped. TODO Find better.
    let !(xo,yo) = foldl (\(xo,yo) u -> 
            let xDif = ux - D.posX u
                yDif = uy - D.posY u
                wDif = ((D.weight u + D.weight unit) / rngCount / D.weight unit) * 0.8
                dist = ((D.radius unit + D.radius u) - (sqrt $ xDif^2 + yDif^2)) * wDif
                angl = atan2 yDif xDif 
            in 
            (xo + cos angl * dist, yo + sin angl * dist)
            ) (0,0) inRng 
    in 
    if rngCount < 4 then
        let a = atan2 (300-uy) (400-ux)
            nx = cos a * 3
            ny = sin a * 3
        in
            unit {D.posX = ux + xo + nx, D.posY = uy + yo + ny}
    else
        unit {D.posX = ux + xo, D.posY = uy + yo}