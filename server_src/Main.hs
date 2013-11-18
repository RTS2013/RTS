module Main where

import Data.Binary (decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B (ByteString)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import GHC.Conc.Sync (ThreadId)
import System.Environment (getArgs)
import System.Timeout (timeout)
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
import Data

mainy = do 
	numPlayers <- fmap (read . head) getArgs :: IO Int
	t_ss <- newTVarIO $ Server 4 [] []
	forkIO $ runServer "0.0.0.0" 8666 $ connectPlayers t_ss
	time <- getCurrentTime
	loop 10 time 1

loop :: Integer -> UTCTime -> Integer -> IO ()
loop fps time steps = do
	putStrLn $ show steps
	timeNow <- getCurrentTime
	threadDelay $ max 0 $ fromIntegral $ 
		(steps * 1000000 `div` fps) - 
		ceiling (diffUTCTime timeNow time * fromInteger 1000000)
	loop fps time (steps + 1)

