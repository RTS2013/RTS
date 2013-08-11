module Sequencing 
( combos
, combor
, combol
, combosl
, combosr
) where

import Data.Sequence
import Data.List(intercalate)
import Data.Char(toLower)
import Data.Foldable(toList)

combos :: String -> [String]
combos = fmap (intercalate " ") . toList . fmap toList . combosl . fromList . words . map toLower

combor :: Seq a -> Seq (Seq a)
combor s = case viewr s of
    EmptyR    -> empty
    (xs :> _) -> xs <| combor xs

combol :: Seq a -> Seq (Seq a)
combol s = case viewl s of
    EmptyL    -> empty
    (_ :< xs) -> xs <| combor xs

combosl :: Seq a -> Seq (Seq a)
combosl s = s <| case viewl s of
    EmptyL    -> empty
    (_ :< xs) -> case viewl xs of
        EmptyL -> empty
        anyOther -> case viewr $ combor s of
            EmptyR    -> combosl xs
            (ys :> _) -> ys >< combosl xs

combosr :: Seq a -> Seq (Seq a)
combosr s = s <| case viewr s of
    EmptyR    -> empty
    (xs :> _) -> case viewr xs of
        EmptyR -> empty
        anyOther -> case viewl $ combol s of
            EmptyL    -> combosr xs
            (_ :< ys) -> ys >< combosr xs