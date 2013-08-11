{-# LANGUAGE OverloadedStrings #-}

module TextParser where

import Parser
import Data.Monoid ((<>))
import Control.Applicative
import qualified Data.Char as C
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

data Token = Token
    T.Text -- data to be parsed
    Int -- column
    Int -- row
    deriving (Show)

type Tokens = [Token]

data Doc = Doc
    T.Text -- text to be parsed
    Int -- column
    Int -- row
    Int -- tab size
    deriving (Show)

(<?) :: P Token Doc a -> T.Text -> P Token Doc a
(<?) p e = P $ \i@(Doc _ c r s) ->
    case runParser p i of
        Value i a -> Value i a
        Error _ -> Error [Token e c r]
        Fatal e -> Fatal e

(?>) :: T.Text -> P Token Doc a -> P Token Doc a
(?>) = flip (<?)

{-
fatal :: T.Text -> P Token Doc a
fatal e = P $ \(Doc _ c r s) -> Fatal [Token e c r]

throw :: T.Text -> P Token Doc a
throw e = P $ \(Doc _ c r s) -> Error [Token e c r]
-}
anyChar :: P Token Doc Char
anyChar = P $ \i@(Doc t c r s) ->
    if T.null t
    then Error [Token "End of file." c r]
    else 
        let hd = T.head t in
        let tl = T.tail t in
        case hd of
            '\n' -> Value (Doc tl 1 (r+1) s) hd
            '\r' -> Value (Doc tl 1 (r+1) s) hd
            '\t' -> Value (Doc tl (c+s) r s) hd
            char -> Value (Doc tl (c+1) r s) hd

next :: P Token Doc a -> P Token Doc a
next p = p <|> (anyChar >> next p)

matchChar :: (Char -> Bool) -> P Token Doc Char
matchChar f = do
    s@(Doc _ c r ts) <- save
    a <- anyChar
    if f a then return a else load s $ Token "Couldn't match character." c r

matchChars :: (Char -> Bool) -> P Token Doc T.Text
matchChars f = fmap T.pack $ some $ matchChar f

space :: P Token Doc String
space = many $ matchChar C.isSpace

char :: Char -> P Token Doc Char
char c = matchChar (==c)

text :: T.Text -> P Token Doc T.Text
text t = case T.uncons t of
    Nothing -> return ""
    Just (c,t) -> do
        char c
        cs <- text t
        return $ c `T.cons` cs

matchText :: (Char -> Bool) -> P Token Doc T.Text
matchText f = fmap T.pack $ some $ matchChar f

alpha :: P Token Doc Char
alpha = matchChar C.isAlpha

digit :: P Token Doc Char
digit = matchChar C.isDigit

natural :: P Token Doc T.Text
natural = fmap T.pack $ some digit <? "Failed to parse natural."

integer :: P Token Doc T.Text
integer = "Failed to parse integer." ?> do
    s <- text "-" <|> return ""
    n <- natural
    return $ s <> n

real :: P Token Doc T.Text
real = "Failed to parse real." ?> do
    i <- integer
    p <- text "." <|> return ""
    n <- natural <|> return ""
    return $ i <> p <> n

number :: P Token Doc T.Text
number = real <|> integer

tokenize :: P Token Doc T.Text -> P Token Doc Tokens
tokenize f = some $ do
    space
    Doc _ c r _ <- save
    x <- f
    return $ Token x c r

parse :: String -> P Token Doc a -> IO (Either T.Text a)
parse s p = do
    c <- TIO.readFile s
    case runParser p $ Doc c 1 1 4 of
        Value _ a -> return . Right $ a
        Error e   -> return . Left . properErrorMessage $ e
        Fatal e   -> return . Left . properErrorMessage $ e
    where
    properErrorMessage :: Tokens -> T.Text
    properErrorMessage = foldl (\m d -> m `T.append` d2m d) ""
        where
        d2m (Token t c r) = 
            "(c/r : " `T.append` 
            T.pack (show c) `T.snoc`
            '/' `T.append`
            T.pack (show r) `T.append`
            ") " `T.append`
            t `T.snoc` '\n'

testParser :: (Show a) => String -> P Token Doc a -> IO ()
testParser s p = parse s p >>= either TIO.putStr print