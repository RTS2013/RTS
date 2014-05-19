{-# OPTIONS -fno-warn-missing-signatures #-}

module MIO.Repa where

import MIO.MIO
import MIO.Privileges ()
import qualified Data.Array.Repa as R

computeP a = Change $! \_ -> R.computeP a
copyP a = Change $! \_ -> R.copyP a
computeUnboxedP a = Change $! \_ -> R.computeUnboxedP a
foldP a b c = Change $! \_ -> R.foldP a b c
sumP a = Change $! \_ -> R.sumP a
selectP a b c = Change $! \_ -> R.selectP a b c