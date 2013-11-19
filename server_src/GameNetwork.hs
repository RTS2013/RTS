{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module GameNetwork (tcpGameServer,PlayerConnection) where

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Unboxed.Mutable as M
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send)
import Network.Socket.ByteString.Lazy
import Data.Binary (decodeOrFail,Binary,get,put,Get)
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO,killThread,myThreadId,ThreadId)
import Control.Concurrent.MVar (putMVar,takeMVar,newEmptyMVar)
import Control.Monad.STM (atomically)
import System.Timeout (timeout)
import Data

type Name = String
type TeamId = Int
data HelloMessage = HelloMessage Name TeamId

instance Binary HelloMessage where
    get = do
        name <- get
        team <- get
        return $ HelloMessage name team
    put = undefined

data PlayerConnection = PlayerConnection Name PlayerStatus Socket Socket SockAddr
data ServerState = ServerState 
    { serverState_teamCounts :: (V.Vector Int)
    , serverState_playerCons :: [PlayerConnection]
    } 

tcpGameServer :: V.Vector Int -> IO [PlayerConnection]
tcpGameServer teamCounts = withSocketsDo $ do
    sync <- newEmptyMVar
    -- let addrFamily = if isSupportedFamily AF_INET6 then AF_INET6 else AF_INET
    localAddr <- fmap head $ getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) 
                                         Nothing (Just "3000")
    sock <- socket (addrFamily localAddr) Stream defaultProtocol
    bind sock (addrAddress localAddr)
    serverStateVar <- newTVarIO $ ServerState teamCounts []
    -- Only accept as many connections as there are potential players
    listen sock $ V.foldl (+) 0 teamCounts
    -- Accept a single player
    let acceptPlayer :: (Socket,SockAddr) -> ThreadId -> IO ()
        acceptPlayer (sockTCP,addr) acceptLoop = do
            -- putStrLn $ "Binding TCP socket to " ++ show addr
            -- bind sockTCP addr
            -- Try to validate player. Give up in 5 seconds.
            putStrLn "Validating connection"
            accepted <- timeout 5000000 $ do
                msg <- recv sockTCP 512
                case decodeOrFail msg of
                    Left _ -> putStrLn "Somebody didn't have a multi-pass"
                    Right (_,_, HelloMessage name team) -> do
                        -- Get next connection
                        putStrLn "Making UDP socket"
                        sockUDP <- socket (addrFamily localAddr) Datagram defaultProtocol
                        putStrLn $ "Binding UDP socket to " ++ show (addrAddress localAddr)
                        bind sockUDP (addrAddress localAddr)
                        putStrLn $ "Connecting UDP socket to " ++ show addr
                        connect sockUDP addr
                        putStrLn $ "Checking for team slots"
                        slotExisted <- atomically $ do
                            (ServerState teamCounts playerList) <- readTVar serverStateVar
                            case teamCounts V.!? team of
                                Nothing -> return False
                                Just count -> 
                                    if count > 0 
                                    then writeTVar serverStateVar (ServerState 
                                        (V.modify (\v -> M.write v team (count-1)) teamCounts)
                                        (PlayerConnection name Playing sockTCP sockUDP addr : playerList))
                                        >> return True
                                    else return False
                        if not slotExisted
                        then putStrLn $ name ++ " couldn't join team " ++ show team
                        else do 
                            putStrLn $ name ++ " joined team " ++ show team 
                            if V.foldl (+) 0 teamCounts /= 0
                            then return ()
                            else do
                                putStrLn "Starting game..."
                                atomically (readTVar serverStateVar) >>= putMVar sync . serverState_playerCons
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