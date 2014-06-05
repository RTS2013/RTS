module Benchmarks where

import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IM
import           Data.List (foldl')
import           Criterion (bench,nf)
import           Criterion.Main (defaultMain)
import           Control.Monad (replicateM_)
import           Control.Monad.Par.IO as P
import           Control.Monad.Par.Class as P
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Time.Clock (diffUTCTime,getCurrentTime)

main2 :: IO ()
main2 = do
    let dummy     = nf id ()
    let dummyMap  = IM.fromList [(x,y) | x <- [0..9999::Int], y <- [0::Int]]
    let mapUpdate = nf (foldl' (\m _ -> IM.update (Just . (+1)) 9999 m) dummyMap) [0..9999::Int]
    let setUnion  = nf (foldl' Set.union Set.empty) $ replicate 500 $ 
                    Set.fromList [(x,y) | x <- [0..19::Int], y <- [0..19::Int]]
    defaultMain [ bench "DUMMY" dummy
                , bench "PAR IO" parIO
                , bench "SEQ IO" seqIO
    			, bench "MAP UPDATE" mapUpdate
    			, bench "SET UNION" setUnion
                ]

main :: IO ()
main = do
    replicateM_ 10 parIO
    putStrLn "Benching parIO ..."
    a <- getCurrentTime
    replicateM_ 10 parIO
    putStrLn "Benching seqIO ..."
    b <- getCurrentTime
    replicateM_ 10 seqIO
    c <- getCurrentTime
    print $ b `diffUTCTime` a
    print $ c `diffUTCTime` b

toDoList :: (P.NFData a) => [IO a] -> IO [a]
toDoList = P.runParIO . inPar . fmap liftIO
    where
    inPar (x:xs) = do
        aVar <- P.spawn x
        ys <- inPar xs
        y <- P.get aVar
        return (y:ys)
    inPar [] = return []

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parIO :: IO ()
parIO = do
    _ <- toDoList $ replicate 8 $ do
        c <- newIORef (0::Int)
        mapM_ (\n -> modifyIORef' c (+fib n)) [1..32]
        readIORef c
    return ()

seqIO :: IO ()
seqIO = do
    _ <- sequence $ replicate 8 $ do
        c <- newIORef (0::Int)
        mapM_ (\n -> modifyIORef' c (+fib n)) [1..32]
        readIORef c
    return ()