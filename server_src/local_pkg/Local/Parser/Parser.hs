{-# LANGUAGE OverloadedStrings #-}

module Local.Parser.Parser
( P(..)
, Result(..)
, save
, load
, choice
, between
) where

import Control.Applicative

data Result e i o 
    = Value i o
    | Error [e]
    | Fatal [e]
    deriving (Show)

newtype P e i o = P {runParser :: i -> Result e i o}

instance Functor (P e i) where
    fmap f p = P $ \i -> case runParser p i of
        Value i o -> Value i $ f o
        Error e -> Error e
        Fatal e -> Fatal e

instance Applicative (P e i) where
    pure a = P $ \i -> Value i a
    (<*>) f g = P $ \i -> case runParser f i of
        Value i f -> case runParser g i of
            Value i o -> Value i $ f o
            Error e -> Error e
            Fatal e -> Fatal e
        Error e -> Error e
        Fatal e -> Fatal e
    (*>) f g = P $ \i -> case runParser f i of
        Value i _ -> runParser g i
        Error e -> Error e
        Fatal e -> Fatal e
    (<*) = flip (*>)

instance Monad (P e i) where
    return a = P $ \i -> Value i a
    (>>=) f g = P $ \i -> case runParser f i of
        Value i o -> runParser (g o) i
        Error e -> Error e
        Fatal e -> Fatal e
    (>>) f g = P $ \i -> case runParser f i of
        Value i _ -> runParser g i
        Error e -> Error e
        Fatal e -> Fatal e

instance Alternative (P e i) where
    empty = P $ \i -> Error []
    (<|>) f g = P $ \i -> case runParser f i of
        Error e -> case runParser g i of
            Error q -> Error $ e ++ q
            valfata -> valfata
        valfata -> valfata
    many f = (do
        x <- f
        xs <- many f
        return $ x:xs) <|> return []
    some f = do
        x <- f
        xs <- many f
        return $ x:xs

save :: P e i i
save = P $ \i -> Value i i

load :: i -> e -> P e i o
load i e = P $ \_ -> Error [e]

choice :: [P e i o] -> P e i o
choice (x:[]) = x
choice (x:xs) = x <|> choice xs

between :: P e i b -> P e i a -> P e i [a]
between g f = do
    x <- f
    xs <- (g >> g `between` f) <|> return []
    return $ x:xs