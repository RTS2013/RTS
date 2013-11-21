module Main where

import qualified Data.Vector.Unboxed as V (fromList)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Data.Time.Clock 
    ( UTCTime
    , NominalDiffTime
    , diffUTCTime
    , getCurrentTime
    , picosecondsToDiffTime )
-- Local code
import Data
import GameNetwork (gameServer)

main = do 
    -- teamCounts <- fmap (read . (!! 0)) getArgs
    putStrLn "Enter the number of players per team (ex. [1,3,2] )"
    teamCounts <- fmap read getLine
    players <- gameServer $ V.fromList teamCounts
    -- Start main loop
    loop 10 stepGame ()

loop :: Integer -> (a -> IO a) -> a -> IO ()
loop fps f world = getCurrentTime >>= actualLoop 1 world
    where
    actualLoop steps world time = do
        newWorld <- f world
        timeNow <- getCurrentTime
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow time * fromInteger 1000000)
        actualLoop (steps + 1) newWorld time 

stepGame :: a -> IO a
stepGame world = undefined