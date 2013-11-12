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
import Network.WebSockets 
	( PendingConnection
	, Connection
	, acceptRequest
	, receiveData
	, runServer )
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

connectPlayers :: TVar Server -> PendingConnection -> IO ()
connectPlayers t_ss pconn = do
	canConnect <- atomically $ do 
		ss <- readTVar t_ss
		if server_numConnections ss < 1 then do
			writeTVar t_ss $ ss {server_numConnections = server_numConnections ss - 1}
			return True
		else
			return False
	if not canConnect
	then putStrLn "No more connections allowed."
	else do
		numConnsLeft <- atomically $ fmap server_numConnections $ readTVar t_ss
		putStrLn $ "Accepting connection. " ++ show numConnsLeft ++ " connection(s) can be made."
		mconn <- timeout 5000000 $ acceptRequest pconn
		case mconn of
			-- Connection couldn't be made in 5 seconds
			Nothing -> atomically $ modifyTVar t_ss
				-- Increment number of connections that can be made
				(\ss -> ss {server_numConnections = server_numConnections ss + 1}) 
			-- Connection was made
			Just conn -> do
				thread <- myThreadId
				atomically $ modifyTVar t_ss 
					(\ss -> ss {server_clients = (numConnsLeft,thread,conn):server_clients ss})
				receiveMessages t_ss conn

receiveMessages :: TVar Server -> Connection -> IO ()
receiveMessages t_ss conn = do
	bits <- receiveData conn :: IO ByteString
	case decodeOrFail bits of
		Left _ -> return ()
		Right (_,_,msg) -> atomically $ modifyTVar t_ss $
			\ss -> ss { server_clientMessages = msg:server_clientMessages ss }