{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.Gloss.Interface.Pure.Game
import Local.WindowSize
import Graphics.Gloss
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.STM.TVar
import Control.Monad (forever)
import Data.Binary (decodeOrFail)
import GUI
import Data

data UI = UI
	{ ui_commands :: [Button World]
	, ui_minimap  :: ()
	, ui_messages :: [String]
	}

data World = World
	{ world_ui :: UI
	, world_terrain :: ()
	}

data FrameState = FrameState

mainy = do 
    winDims <- getWindowSize
    let renderButtons btns = pictures $ map button2Picture btns
        buttons = fst $ arrange Northwest East winDims
            [ makeColorButton (64,64) red id
            , makeColorButton (64,64) blue id
            , makeColorButton (64,64) yellow id
            , makeColorButton (64,64) green id
            ]
    play (FullScreen winDims) 
        black -- Background color
        60 -- FPS
        (World (UI buttons () []) ()) -- Starting world state
        (\world -> pictures []) -- Render world state
        eventHandler
        (\_ world -> world)

renderWorld :: World -> Picture
renderWorld world = undefined

-- LEFT CLICK
eventHandler (EventKey (MouseButton LeftButton) Down _ (x,y)) world = 
	case selectButton x y $ ui_commands $ world_ui world of
	Just f -> f world
	Nothing -> world
eventHandler (EventMotion (x,y)) world = world
-- EVERYTHING ELSE
eventHandler _ world = world

frameStateReceiver :: TVar FrameState -> IO ()
frameStateReceiver tvar = do
	let addrFamily = if isSupportedFamily AF_INET6 then AF_INET6 else AF_INET
	addrInfo <- getAddrInfo (Just defaultHints) Nothing Nothing
	case addrInfo of
		[ ] -> putStrLn "You have no address apparently."
		x:_ -> do
			sock <- socket addrFamily Datagram defaultProtocol
			bind sock (addrAddress x)
			forever $ do
				bytes <- recv sock 65536
				case decodeOrFail bytes of
					Left _ -> putStrLn "Somebody is trying to send you bad data."
					Right (_,_,i :: Int) -> return ()