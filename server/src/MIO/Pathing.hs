{-# LANGUAGE Trustworthy #-}

module MIO.Pathing where

import MIO.MIO
import MIO.Privileges ()
import Pathing (setGroupsM,setGroupsP)
import Data.Vector.Unboxed (Vector)

setGroupsMIO wh isOpenM setCornersM = 
	Change $! \s -> do
		let isOpenIO a = (change (isOpenM a)) s
		let setCornersIO a v = (change (setCornersM a v)) s
		setGroupsM wh isOpenIO setCornersIO

setGroupsPIO wh isOpen = 
	Change $! \s -> setGroupsP wh isOpen