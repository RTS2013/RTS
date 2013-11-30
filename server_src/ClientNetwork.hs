module ClientNetwork where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send)
import Network.Socket.ByteString.Lazy
import Data.Binary (decodeOrFail,encode)
import Data

connectToServer :: IO (Socket,Socket)
connectToServer = withSocketsDo $ do
    putStrLn "Enter server address"
    serverIP <- getLine
    addrInfos <- getAddrInfo Nothing (Just serverIP) (Just "3000")
    let serverAddr = head addrInfos
    putStrLn "Enter the team you want to play"
    yourTeam <- fmap read getLine
    putStrLn "Enter your name"
    yourName <- getLine
    putStrLn "Enter a secret"
    yourSecret <- getLine
    -- Connect TCP socket
    tcpSock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect tcpSock (addrAddress serverAddr)
    -- Send hello message
    send tcpSock $ LB.toStrict $ encode $ HelloMessage yourTeam yourName yourSecret
    -- Connect UDP socket
    udpSock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    connect udpSock (addrAddress serverAddr)
    putStrLn "Connected to server"
    return (tcpSock,udpSock)

main = do
    (tcp,udp) <- connectToServer
    close tcp
    close udp
    putStrLn "Closed stuff"