{-# LANGUAGE Unsafe #-}

module RIO.RIO where

newtype RIO privilege a = RIO {toIO :: (IO a)}