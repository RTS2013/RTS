module Main where

import qualified Data.Vector.Unboxed as V (fromList)
import Data.Binary (decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B (ByteString)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import GHC.Conc.Sync (ThreadId)
import System.Environment (getArgs)
import System.Timeout (timeout)
import Control.Concurrent.MVar (newEmptyMVar,takeMVar)
import Network.Info (getNetworkInterfaces,mac)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send)
import Network.Socket.ByteString.Lazy
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
import GameNetwork

main = do 
    getNetworkInterfaces >>= print . mac . head
    -- numPlayers <- fmap (read . head) getArgs :: IO Int
    -- teamCounts <- fmap read getLine
    let teamCounts = [1]
    playerCons <- tcpGameServer $ V.fromList teamCounts
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

test :: IO ()
test = withSocketsDo $ do 
    putStrLn "Enter Server Address"
    serverAddress <- getLine
    addrinfos <- getAddrInfo Nothing (Just serverAddress) (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    sClose sock