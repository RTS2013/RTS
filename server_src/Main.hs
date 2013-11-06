module Main where

import System.Environment 
	( getArgs )
import Network.WebSockets 
	( PendingConnection
	, Connection
	, acceptRequest
	, receiveData
	, runServer )
import Data.Binary 
	( decodeOrFail )
import Data.Binary.Get 
	( ByteOffset )
import Data.ByteString.Lazy 
	( ByteString )
import qualified Data.ByteString.Char8 as B 
	( ByteString )
import Control.Monad.STM 
	( atomically )
import Control.Concurrent 
	( forkIO
	, threadDelay )
import Control.Concurrent.STM.TVar 
	( TVar
	, modifyTVar
	, readTVar
	, newTVarIO )
import Data.Time.Clock 
	( UTCTime
	, NominalDiffTime
	, diffUTCTime
	, getCurrentTime
	, picosecondsToDiffTime )
import Data

main = do 
	numPlayers <- fmap (read . head) getArgs :: IO Int
	t_ss <- newTVarIO $ ServerState 4 []
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

connectPlayers :: TVar ServerState -> PendingConnection -> IO ()
connectPlayers t_ss pconn = do
	canConnect <- atomically $ do 
		ss <- readTVar t_ss
		return $ server_num_connections ss < 1
	if canConnect
	then do
		atomically $ modifyTVar t_ss $ 
			\ss -> ss { server_num_connections = server_num_connections ss - 1}
		acceptRequest pconn >>= receiveMessages t_ss
	else return ()

receiveMessages :: TVar ServerState -> Connection -> IO ()
receiveMessages t_ss conn = do
	bits <- receiveData conn :: IO ByteString
	case decodeOrFail bits of
		Left _ -> return ()
		Right (_,_,msg) -> atomically $ modifyTVar t_ss $
			\ss -> ss { server_client_messages = msg:server_client_messages ss }