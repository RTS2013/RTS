module Main where

import System.Random
import Local.Matrices
import Local.Pathing
import qualified Data.Vector as V
import Graphics.Gloss

data Tile = Tile Bool [(Int,Int)] deriving (Show)

main = do
  {-rs <- fmap (randomRs (0,255)) getStdGen
  let xs = take 5000 rs
    ys = take 5000 $ drop 5000 rs
    xys = zip xs ys
    m = foldl (\m (x,y) -> update2D 
      (\_ -> Tile False []) m x y) 
      (make2D 256 256 $ Tile True []) xys
    -}
  let m = foldl (\m (x,y) -> update2D x y
          (\_ -> Tile False []) m
        ) 
        (make2D 256 256 $ Tile True []) 
        [(x,y) 
        | x <- [0..255]
        , y <- [0..255]
        , x `mod` 10 < 8
        , y `mod` 10 < 8
        , (x + y) `mod` 20 < 16
        ]
      (corners,matrix) = setCorners m (\cs (Tile b _) -> Tile b cs)
  display (InWindow "Hoodabaga" (800,600) (32,32)) white $ pictures $ 
    V.ifoldl' (\ts y xv -> ts ++ 
    V.ifoldl' (\ts x (Tile b xs) -> 
        if not $ null xs 
        then square red x y:ts 
        else if b 
           then square white x y:ts 
           else square black x y:ts) [] xv
      ) [] matrix

square :: Color -> Int -> Int -> Picture
square c x y = translate (fromIntegral x * 10) (fromIntegral y * 10) $ 
  color c $ polygon [(0,0),(0,10),(10,10),(10,0)]