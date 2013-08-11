{-# LANGUAGE OverloadedStrings #-}

module TPTest where

import Parser
import TextParser
import TokenParser
import Control.Applicative
import qualified Data.Char as C
import qualified Data.Text.Lazy as T

funcs :: P Token Tokens [(T.Text,[T.Text],[T.Text])]
funcs = some $ do
    name <- indented (==1) $ matchToken (T.all $ \c -> C.isAlpha c || C.isDigit c) 
             <|> matchToken (T.all C.isSymbol)
    args <- many $ indented (>1) $ matchToken (/= "=")
    indented (>1) (token "=") <|> fatal "No '=' after previous function declaration."
    expr <- (some $ indented (>1) $ matchToken (/= "=")) <|> fatal "Previous function is missing an expression."
    return (name,args,expr)

validTokenText :: P Token Doc T.Text
validTokenText = choice [number,identifier,void,operator,singleChar,singlePunc]
    where
    identifier = fmap T.pack $ some $ alpha <|> digit
    operator = matchChars $ flip elem "=:\\|;+*-/<>~!@#$%^&?"
    void = matchChars (=='_')
    singleChar = do
        char '\''
        b <- text "\\" <|> return ""
        c <- anyChar
        char '\''
        return $ b `T.snoc` c
    singlePunc = fmap T.singleton $ matchChar $ flip elem "()[]{}'\",`"
    

main = testParser "tokentest" $ tokenize validTokenText >>= return . runParser funcs