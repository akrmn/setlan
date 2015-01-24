-- CI 3725 Traductores e interpretadores
-- Project part 1 lexer
-- Members:
--         Moises Ackerman 11-10005
--         Carlos Ferreira 11-10323

module Main (main) where

import Lexer
import Tokens
import System.Environment
import System.IO

-- Verification of lexer
lexr :: String -> IO ()
lexr text = do
    let toks = alexScanTokens text
    if noError toks
        then mapM_ print toks
        else printError toks

-- Prints the errors if they exist
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
