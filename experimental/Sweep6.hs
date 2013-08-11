module Sweep6 where

import Matrices

data Sweep a = Empty
             | Space a Int Int
               (Sweep a) -- N
               (Sweep a) -- NE
               (Sweep a) -- SE
               (Sweep a) -- S
               (Sweep a) -- SW
               (Sweep a) -- NW

sweep :: Int -> Int -> V.Vector (V.Vector a) -> Sweep a
sweep x y v = case get2D v x y of
    Nothing -> Empty
    Just ok -> Space ok x y
        (sweep x (y+1) v)     -- N
        (sweep (x+1) (y+1) v) -- NE
        (sweep (x+1) y v)     -- SE
        (sweep x (y-1) v)     -- S
        (sweep (x-1) y v)     -- SW
        (sweep (x-1) (y+1) v) -- NW