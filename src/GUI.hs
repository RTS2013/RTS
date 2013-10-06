import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer.Strict
import Data.Map.Strict
import WindowSize (getWindowSize)


data Button a = Button {btn_x, btn_y, btn_w, bth_h :: Float, btn_clicked :: (Float,Float) -> a}

type GUI a = [Button a]

data World = World

mkButton :: (Picture,Picture) -> (Either (Float,Float) Bool -> a) -> (Float,Float) -> (Float,Float) -> Button a
mkButton = undefined

main = do
	let initialWorld = World
	window_size <- getWindowSize
	playIO 
		(FullScreen window_size) 
		black 
		60 
		initialWorld
		worldPicture
		worldEvent
		worldStep
		
worldPicture :: World -> IO Picture
worldPicture w = return (color white $ Circle 80)

worldEvent :: Event -> World -> IO World
worldEvent _ = return . id

worldStep :: Float -> World -> IO World
worldStep time_delta = return . id