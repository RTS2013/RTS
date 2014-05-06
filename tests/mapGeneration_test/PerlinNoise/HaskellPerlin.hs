{-# LANGUAGE TypeOperators #-}

import Numeric.Noise.Perlin
import System.Random (randomIO,randomIO,randomRs,newStdGen,mkStdGen)
import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor8)
import qualified Data.Array.Repa as R

type Seed = Int

rangeMap :: (Ord a) => b -> [(a,b)] -> a -> b
rangeMap def rs x = case dropWhile (\(a,_) -> x > a) rs of
        (_,b):_ -> b
        _______ -> def

main = do
    heightSeed <- randomIO :: IO Int
    treeSeed <- randomIO :: IO Int
    let heightP = perlin heightSeed 16 (1/128) (1/2)
        treeP = perlin treeSeed 16 (1/128) (1/2)
        w = 1024 :: Int
        h = 1024 :: Int
        shape = (R.Z R.:. w R.:. h)
    heightArray <- R.computeP $ R.fromFunction shape
            (\(R.Z R.:.x R.:. y) -> ( fromIntegral x
                                    , fromIntegral y
                                    , rangeMap 255 [(-0.9,0),(0.25,130)] $ 
                                      noiseValue heightP (fromIntegral x, fromIntegral y, 0)
                                    )
            ) :: IO (R.Array R.U R.DIM2 (Float,Float,Int))
    let heightPic = R.map (\(x,y,z) -> scale 1 1 
                                     $ translate (x - fromIntegral w / 2) (y - fromIntegral h / 2) 
                                     $ color (makeColor8 z z z 255) 
                                     $ rectangleSolid 1 1
                    ) heightArray
    let trees = randomPerlin heightSeed treeSeed (0,3) (w,h)
        treePic = shapeMap (\x y z -> let fx = fromIntegral x
                                          fy = fromIntegral y in
                                     scale 1 1 
                                   $ translate (fx - fromIntegral w / 2) (fy - fromIntegral h / 2) 
                                   $ (if z>3.25 then color (makeColor8 0 255 0 255) else color (makeColor8 0 0 0 255))
                                   $ rectangleSolid 1 1
                    ) trees
    display (FullScreen (1600, 1200)) black $ pictures $ R.toList heightPic
    -- display (InWindow "Perlin Test" (1600, 1200) (0, 0)) black $ pictures $ R.toList treePic

shapeMap f array = R.fromFunction (R.extent array) (\sh@(R.Z R.:.x R.:. y) -> f x y $ array R.! sh)

randomPerlin :: Seed -- Perlin Seed
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