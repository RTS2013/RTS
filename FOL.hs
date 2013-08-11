module FOL where

-- Match all elements
uq :: [a] -> (a -> Bool) -> Bool
uq = flip all

-- Up to n elements don't match
uq_n :: Int -> [a] -> (a -> Bool) -> Bool
uq_n n xs f = (n>=) . length $ filter (not . f) xs

-- Exactly n elements don't match
uq_e :: Int -> [a] -> (a -> Bool) -> Bool
uq_e n xs f = (n==) . length $ filter (not . f) xs

-- Match any elements
ex :: [a] -> (a -> Bool) -> Bool
ex = flip any

-- Match n or more elements
ex_n :: Int -> [a] -> (a -> Bool) -> Bool
ex_n n xs f = (n<=) . length $ filter f xs

-- Match exactly n elements
ex_e :: Int -> [a] -> (a -> Bool) -> Bool
ex_e n xs f = (n==) . length $ filter f xs