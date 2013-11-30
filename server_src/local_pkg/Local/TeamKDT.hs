{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- module Local.KDT (KDT(),make,inRange,empty) where

import qualified Data.Vector as V
import qualified Local.KDT as KDT
import GHC.Conc.Sync (par,pseq)


data TeamKDT n a = TeamKDT (V.Vector (KDT.KDT n a)) (V.Vector (KDT.KDT n a))

make :: (Floating n, Ord n) => [a -> n] -> V.Vector [a] -> TeamKDT n a
make fs = TeamKDT . V.map (KDT.makePar fs)

inRange