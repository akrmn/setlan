module Main (main) where

import Lexer
import Tokens
import System.Environment
import System.IO

isError :: Token -> Bool
isError t
    = case t of
        TokenError _ _ -> True
        _              -> False

noError :: [Token] -> Bool
noError l = null $ listErrors l

listErrors :: [Token] -> [Token]
listErrors l = filter isError l

lexr :: String -> IO ()
lexr text = do
    let toks = alexScanTokens text
    if noError toks
        then mapM_ print toks
        else printError toks

printError :: [Token] -> IO ()
printError s = do
	mapM_ print $ listErrors s

main :: IO ()
main = do
    args <- getArgs
    s <- if length args == 0
        then getContents
        else hGetContents =<< (openFile (args!!0) ReadMode)
    lexr s
