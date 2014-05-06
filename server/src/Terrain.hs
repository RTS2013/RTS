{-# LANGUAGE TypeOperators #-}

module Terrain where

import Numeric.Noise.Perlin
import System.Random (randomRs,mkStdGen)
import qualified Data.Array.Repa as R

type Seed = Int

rangeMap :: (Ord a) => b -> [(a,b)] -> a -> b
rangeMap def rs x = case dropWhile (\(a,_) -> x > a) rs of
    (_,b):_ -> b
    _       -> def

randomPerlin 
    :: Seed -- Perlin Seed
    -> Seed -- Random Seed
    -> (Double,Double) -- Random Range
    -> (Int,Int) -- Matrix Width & Height
    -> R.Array R.U R.DIM2 Double
randomPerlin pSeed rSeed range (w,h) = R.fromListUnboxed shape zips
    where
    perl = perlin pSeed 16 (1/128) (1/2)
    shape = R.ix2 w h
    rnds = randomRs range $ mkStdGen rSeed
    zips = zipWith (\(x,y) rnd -> rnd + noiseValue perl (fromIntegral x, fromIntegral y, 0)) 
                       [(x,y) | x <- [0..w-1], y <- [0..h-1]]
                       rnds 