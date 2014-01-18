{-# LANGUAGE Unsafe #-}

module RIO.RIO where
	
newtype RIO a = RIO (IO a)