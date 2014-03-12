{-# LANGUAGE Unsafe #-}

module MIO.MIO where

newtype Change     a = Change   { change :: IO a }
newtype Behavior s a = Behavior { behave :: s -> IO a }
newtype Trainer  s a = Trainer  { train  :: s -> IO a }