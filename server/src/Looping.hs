module Looping where

import Control.Concurrent (threadDelay)
import Data.Int (Int64)
import Data.Time.Clock (diffUTCTime,getCurrentTime)

loopDelta :: (Floating f) => a -> (a -> IO a) -> (f -> a -> a) -> (a -> IO a) -> IO ()
loopDelta initGame acceptInput stepGame renderGame = getCurrentTime >>= actualLoop initGame
    where
    actualLoop game lastTime = do
        timeNow <- getCurrentTime
        let delta = fromRational . toRational $ diffUTCTime timeNow lastTime
        newGame <- acceptInput game >>= renderGame . stepGame delta
        actualLoop newGame  timeNow

loopFPS :: (Floating f) => Int64 -> a -> (f -> a -> IO a) -> IO ()
loopFPS fps initGame stepGame = getCurrentTime >>= \t -> actualLoop initGame 1 t t
    where
    frame = 1 / fromIntegral fps
    actualLoop game steps startTime lastTime = do
        timeNow <- getCurrentTime
        let delta = fromRational . toRational $ diffUTCTime timeNow lastTime
        print (delta :: Double)
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow startTime * 1000000)
        newGame <- stepGame frame game
        actualLoop newGame (steps + 1) startTime timeNow