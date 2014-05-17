{-# LANGUAGE TypeOperators #-}

module Terrain where

import Numeric.Noise.Perlin
import System.Random (randomRs,mkStdGen)
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as R (Unbox)

type Seed = Int

rangeMap :: (Ord a) => b -> [(a,b)] -> a -> b
rangeMap def rs x = case dropWhile (\(a,_) -> x > a) rs of
    (_,b):_ -> b
    _       -> def

randomPerlin 
    :: (R.Unbox b)
    => Seed -- Perlin Seed
    -> Seed -- Random Seed
    -> (Double,Double) -- Random Range
    -> (Int,Int) -- Matrix Width & Height
    -> (Double -> b)
    -> R.Array R.U R.DIM2 b
randomPerlin pSeed rSeed range (w,h) f = R.fromListUnboxed shape zips
    where
    perl = perlin pSeed 32 (1/128) (1/2)
    shape = R.ix2 w h
    rnds = randomRs range $ mkStdGen rSeed
    zips = zipWith (\(x,y) rnd -> f $ rnd + noiseValue perl (fromIntegral x, fromIntegral y, 0)) 
                       [(x,y) | x <- [0..w-1], y <- [0..h-1]]
                       rnds 

perlinMap :: (R.Unbox a)
          => Seed
          -> (Int,Int)
          -> (Double -> a)
          -> IO (R.Array R.U R.DIM2 (Int,Int,a))
perlinMap seed (w,h) f = R.computeUnboxedP 
    $ R.fromFunction (R.ix2 w h) (\sh@(R.Z R.:. x R.:. y) -> (x,y, f $ perl (fromIntegral x, fromIntegral y, 0)))
    where
    perl xyz = noiseValue (perlin seed 16 (1/128) (1/2)) xyz