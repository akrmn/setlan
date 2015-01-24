-- CI3725 Traductores e interpretadores
-- Project part 1 - Lexer
-- Members:
--         Moises Ackerman 11-10005
--         Carlos Ferreira 11-10323

module Main (main) where

import Lexer
import Tokens
import System.Environment
import System.IO

isError :: Token -> Bool
-- Checks whether a Token has TokenError constructor
isError t
    = case t of
        TokenError _ _ -> True
        _              -> False

noError :: [Token] -> Bool
-- Checks whether a list of tokens contains a TokenError
noError l = null $ listErrors l

listErrors :: [Token] -> [Token]
-- Returns only the TokenErrors in a [Token]
listErrors l = filter isError l

lexr :: String -> IO ()
-- Calls the Alex token scanner and then prints found tokens. If there are any
-- TokenErrors, it only prints those.
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
