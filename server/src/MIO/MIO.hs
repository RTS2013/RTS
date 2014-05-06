{-# LANGUAGE Unsafe, GeneralizedNewtypeDeriving, RankNTypes #-}

module MIO.MIO where

newtype Behavior s a = Behavior { behave :: s -> IO a }
newtype Change   s a = Change   { change :: s -> IO a }