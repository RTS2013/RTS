{-# LANGUAGE ScopedTypeVariables #-}

module ActorAttributes
( Attributes()
, newAttrs
, getAttr
, setAttr
, modifyAttr
) where

import Control.DeepSeq (NFData(..))
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

newtype Attributes i a = Attributes (V.Vector a)

newAttrs :: forall i a. (Bounded i, Enum i, M.Unbox a) => a -> Attributes i a
newAttrs a = Attributes $ V.replicate (((fromEnum :: (Enum i) => i -> Int) maxBound) + 1) a

getAttr :: (Enum i, M.Unbox a) => Attributes i a -> i -> a
getAttr (Attributes stats) stat = stats V.! fromEnum stat

setAttr :: (Enum i, M.Unbox a) => Attributes i a -> i -> a -> Attributes i a
setAttr (Attributes stats) stat value = Attributes $ 
	V.modify (\vec -> M.write vec (fromEnum stat) value) stats

modifyAttr :: (Enum i, M.Unbox a) => Attributes i a -> i -> (a -> a) -> Attributes i a
modifyAttr (Attributes stats) stat f = Attributes $
	V.modify (\vec -> M.write vec (fromEnum stat) $ f (stats V.! fromEnum stat)) stats 

instance NFData (Attributes i a) where rnf = (`seq` ())