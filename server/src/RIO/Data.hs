{-# LANGUAGE Trustworthy #-}

module RIO.Data where
import Data
import qualified RIO.Grid as Grid

makeTeam :: teamS -> (Int,Int) -> RIO (Team gameS teamS unitS tileS)
makeTeam teamS size = do 
	grid <- Grid.make size False