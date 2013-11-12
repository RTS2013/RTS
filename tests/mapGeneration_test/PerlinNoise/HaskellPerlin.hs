import Numeric.Noise.Perlin
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor8)

main = do
    let p = perlin 1 5 (1/8) (1/2)
        m = map (noiseValue p) [(x,y,0) | x <- [0..99], y <- [0..99]]
        n = rangeMap 50 [(-0.5,0),(0,100),(5000,190)] m
        xs = map (\((x,y),z) -> (x, y, z)) $ zip [(x,y) | x <- [0..99], y <- [0..99]] n
    display (InWindow "Perlin Haskell" (1000,1000) (0,0)) black $ pictures $ 
        map (\(x,y,z) -> scale 10 10 $ translate (x - 50) (y - 50) $ color (makeColor8 z z z 255) $ rectangleSolid 1 1) xs

rangeMap :: (Ord a) => b -> [(a,b)] -> [a] -> [b]
rangeMap def rs xs = map (\x -> 
        let rs' = dropWhile (\(a,_) -> x > a) rs in
        if null rs' then def else snd (head rs')
    ) xs