module Main where

import Graphics.Gloss.Interface.Pure.Game
import GUI
import Local.WindowSize
import Graphics.Gloss
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent.STM.TVar

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
	sock <- socket (AF_INET6 serveraddr) Datagram defaultProtocol
	return ()