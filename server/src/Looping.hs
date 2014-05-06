module Looping where

import Control.Concurrent (threadDelay)
import Data.Int (Int64)
import Data.Time.Clock (diffUTCTime,getCurrentTime)

loopDelta :: a -> (a -> IO a) -> (Float -> a -> a) -> (a -> IO a) -> IO ()
loopDelta initGame acceptInput stepGame renderGame = getCurrentTime >>= actualLoop initGame
    where
    actualLoop game lastTime = do
        timeNow <- getCurrentTime
        let delta = fromRational . toRational $ diffUTCTime timeNow lastTime
        newGame <- acceptInput game >>= renderGame . stepGame delta
        actualLoop newGame  timeNow

loopFPS :: Int64 -> a -> (Float -> a -> IO a) -> IO ()
loopFPS fps initGame stepGame = getCurrentTime >>= \t -> actualLoop initGame 1 t t
    where
    actualLoop game steps startTime lastTime = do
        timeNow <- getCurrentTime
        let delta = fromRational . toRational $ diffUTCTime timeNow lastTime
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow startTime * 1000000)
        newGame <- stepGame delta game
        actualLoop newGame (steps + 1) startTime timeNow