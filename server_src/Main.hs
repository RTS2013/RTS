module Main where

import qualified Data.Vector.Unboxed as V (fromList)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket.ByteString.Lazy (recv)
import Network.Socket.ByteString (send)
import Network.Socket hiding (send, recv)
import Data.Binary (decodeOrFail,encode)
import Data.Binary.Get (ByteOffset)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import GHC.Conc.Sync (ThreadId)
import System.Environment (getArgs)
import System.Timeout (timeout)
import Control.Concurrent.MVar (newEmptyMVar,takeMVar,putMVar)


import Control.Concurrent.STM.TVar 
    ( TVar
    , modifyTVar
    , readTVar
    , newTVarIO 
    , writeTVar )
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
    -- teamCounts <- fmap read getArgs
    putStrLn "Enter the number of players per team (ex. [1,3,2] )"
    teamCounts <- fmap read getLine
    players <- gameServer $ V.fromList teamCounts
    -- Start main loop
    getCurrentTime >>= loop 10 1

loop :: Integer -> Integer -> UTCTime -> IO ()
loop fps steps time = do
    putStrLn $ show steps
    timeNow <- getCurrentTime
    threadDelay $ max 0 $ fromIntegral $ 
        (steps * 1000000 `div` fps) - 
        ceiling (diffUTCTime timeNow time * fromInteger 1000000)
    loop fps (steps + 1) time 