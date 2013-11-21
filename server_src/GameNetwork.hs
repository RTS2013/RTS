{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module GameNetwork (gameServer) where

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Unboxed.Mutable as M
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy (recv)
import Data.Binary (decodeOrFail,Binary,get,put,Get)
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO,killThread,myThreadId,ThreadId)
import Control.Concurrent.MVar (putMVar,takeMVar,newEmptyMVar)
import Control.Monad.STM (atomically)
import System.Timeout (timeout)
import Data

data ServerState = ServerState 
    { serverState_teamCounts :: (V.Vector Int)
    , serverState_players :: [Player]
    } 

gameServer :: V.Vector Int -> IO [Player]
gameServer teamCounts = withSocketsDo $ do
    sync <- newEmptyMVar
    -- let addrFamily = if isSupportedFamily AF_INET6 then AF_INET6 else AF_INET
    localAddr <- fmap head $ getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) 
                                         Nothing 
                                         (Just "3000")
    sock <- socket (addrFamily localAddr) Stream defaultProtocol
    bind sock (addrAddress localAddr)
    serverStateVar <- newTVarIO $ ServerState teamCounts []
    -- Only accept as many connections as there are potential players
    listen sock $ V.foldl (+) 0 teamCounts
    -- Accept a single player
    let acceptPlayer :: (Socket,SockAddr) -> ThreadId -> IO ()
        acceptPlayer (sockTCP,addr) acceptLoop = do
            -- Try to validate player. Give up in 5 seconds.
            accepted <- timeout 5000000 $ do
                msg <- recv sockTCP 512
                case decodeOrFail msg of
                    Left _ -> putStrLn "Somebody didn't have a multi-pass"
                    Right (_,_, HelloMessage team name secret) -> do
                        -- Get next connection
                        sockUDP <- socket (addrFamily localAddr) Datagram defaultProtocol
                        connect sockUDP addr
                        slotExisted <- atomically $ do
                            (ServerState teamCounts playerList) <- readTVar serverStateVar
                            case teamCounts V.!? team of
                                Nothing -> return False
                                Just count -> 
                                    if count > 0 
                                    then writeTVar serverStateVar (ServerState 
                                        (V.modify (\v -> M.write v team (count-1)) teamCounts)
                                        (Player team name secret Playing sockTCP sockUDP : playerList))
                                        >> return True
                                    else return False
                        if not slotExisted
                        then putStrLn $ name ++ " couldn't join team " ++ show team
                        else do 
                            putStrLn $ name ++ " joined team " ++ show team 
                            ServerState vec _ <- atomically $ readTVar serverStateVar
                            if V.foldl (+) 0 vec /= 0
                            then putStrLn $ show (V.foldl (+) 0 vec) ++ " player(s) left to join"
                            else do
                                putStrLn "Starting game..."
                                atomically (readTVar serverStateVar) 
									>>= putMVar sync . serverState_players
                                killThread acceptLoop
            case accepted of
                Just __ -> return ()
                Nothing -> return ()
        -- Loop of accepting new players
        acceptPlayers :: IO ()
        acceptPlayers = do
            putStrLn "Waiting for connection"
            connection <- accept sock
            putStrLn $ "Accepted connection from " ++ show (snd connection)
            myThreadId >>= forkIO . acceptPlayer connection
            acceptPlayers
    forkIO acceptPlayers
    takeMVar sync