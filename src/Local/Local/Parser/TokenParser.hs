{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Local.Parser.TokenParser where

import Local.Parser.Parser
import Local.Parser.TextParser (Token(..),Tokens)
import Data.Monoid ((<>))
import Control.Applicative
import qualified Data.Char as C
import qualified Data.Text.Lazy as T

(<?) :: P Token Tokens a -> T.Text -> P Token Tokens a
(<?) p e = P $ \i -> case i of
    [  ] -> Error [Token "End of file." 0 0]
    x:xs -> case runParser p (x:xs) of
        Value xs a -> Value xs a
        Error _ -> case x of Token _ c r -> Error [Token e c r]
        Fatal e -> Fatal e
    
(?>) :: T.Text -> P Token Tokens a -> P Token Tokens a
(?>) = flip (<?)

fatal :: T.Text -> P Token Tokens a
fatal e = P $ \i -> case i of
    [  ] -> Fatal [Token "End of file." 0 0]
    x:xs -> case x of Token _ c r -> Fatal [Token e c r]

throw :: T.Text -> P Token Tokens a
throw e = P $ \i -> case i of
    [  ] -> Error [Token "End of file." 0 0]
    x:xs -> case x of Token _ c r -> Error [Token e c r]

anyToken :: P Token Tokens Token
anyToken = P $ \i -> case i of
    x:xs -> Value xs x
    [  ] -> Error [Token "End of file." 0 0]

token :: T.Text -> P Token Tokens T.Text
token txt = do
    s <- save
    t@(Token txt' c r) <- anyToken
    if txt' == txt
    then return txt
    else load s $ Token ("Couldn't match " <> txt <> ".") c r

matchToken :: (T.Text -> Bool) -> P Token Tokens T.Text
matchToken f = do
    s <- save
    t@(Token txt c r) <- anyToken
    if f txt
    then return txt
    else load s $ Token ("Couldn't match " <> txt <> ".") c r

parens :: P Token Tokens a -> P Token Tokens a
parens f = do
    token "(" <? "Missing opening '('."
    v <- f
    token ")" <? "Missing closing ')'."
    return v

square :: P Token Tokens a -> P Token Tokens a
square f = do
    token "[" <? "Missing opening '['."
    v <- f
    token "]" <? "Missing closing ']'."
    return v

curly :: P Token Tokens a -> P Token Tokens a
curly f = do
    token "{" <? "Missing opening '{'."
    v <- f
    token "}" <? "Missing closing '}'."
    return v

-- Returns the column and the row of the next token
position :: P Token Tokens (Int,Int)
position = P $ \i -> case i of
        x:xs -> case x of Token _ c r -> Value (x:xs) (c,r)
        [  ] -> Error [Token "End of file." 0 0]

-- Returns an error if the next token isn't indented correctly
indented :: (Int -> Bool) -> P Token Tokens a -> P Token Tokens a
indented ind p = do
    (c,r) <- position
    if ind c
    then p
    else P $ \i -> Error [Token "Bad indentation." c r]

indent :: P Token Tokens Int
indent = P $ \i -> case i of
        x:xs -> case x of Token _ c _ -> Value (x:xs) c
        [  ] -> Error [Token "End of file." 0 0]