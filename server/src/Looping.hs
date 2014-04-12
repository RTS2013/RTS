module Looping where

import Control.Concurrent (threadDelay)
import Data.Int (Int64)
import Data.Time.Clock (diffUTCTime,getCurrentTime)

loopImpure :: Int64 -> (a -> IO ()) -> a -> IO ()
loopImpure fps f game = getCurrentTime >>= actualLoop 1
    where
    actualLoop steps time = do
        f game
        timeNow <- getCurrentTime
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow time * 1000000)
        actualLoop (steps + 1) time 

loop :: Int64 -> a -> (a -> IO a) -> (a -> a) -> (a -> IO a) -> IO ()
loop fps initGame acceptInput stepGame renderGame = getCurrentTime >>= actualLoop initGame 1
    where
    actualLoop game steps startTime = do
        newGame <- acceptInput game >>= renderGame . stepGame
        timeNow <- getCurrentTime
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow startTime * 1000000)
        actualLoop newGame (steps + 1) startTime

loopDelta :: a -> (a -> IO a) -> (Double -> a -> a) -> (a -> IO a) -> IO ()
loopDelta initGame acceptInput stepGame renderGame = getCurrentTime >>= actualLoop initGame
    where
    actualLoop game lastTime = do
        timeNow <- getCurrentTime
        let delta = fromRational . toRational $ diffUTCTime timeNow lastTime
        newGame <- acceptInput game >>= renderGame . stepGame delta
        actualLoop newGame  timeNow