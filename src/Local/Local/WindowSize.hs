module Local.WindowSize (getWindowSize) where

import Graphics.UI.GLUT.State
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.CoordTrans

getWindowSize :: (Num n) => IO (n,n)
getWindowSize = do
	Size w h <- get screenSize
	return (fromInteger $ toInteger w, fromInteger $ toInteger h)