module FOL where

-- Match all elements
uq :: [a] -> (a -> Bool) -> Bool
uq = flip all

-- Match all but n elements
uq_n :: (Int -> Bool) -> [a] -> (a -> Bool) -> Bool
uq_n n xs f = n . length $ filter (not . f) xs

-- Match any elements
ex :: [a] -> (a -> Bool) -> Bool
ex = flip any

-- Match n elements
ex_n :: (Int -> Bool) -> [a] -> (a -> Bool) -> Bool
ex_n n xs f = n . length $ filter f xs
