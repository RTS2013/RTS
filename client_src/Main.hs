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

main = do 
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
        renderWorld
        eventHandler
        (\_ world -> world)

renderWorld :: World -> Picture
renderWorld = pictures . map button2Picture . ui_commands . world_ui

-- LEFT CLICK
eventHandler (EventKey (MouseButton LeftButton) Down _ (x,y)) world = 
	case selectButton x y $ ui_commands $ world_ui world of
	Just f -> f world
	Nothing -> world
eventHandler (EventMotion (x,y)) world = world
-- EVERYTHING ELSE
eventHandler _ world = world