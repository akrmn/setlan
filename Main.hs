module Main (main) where

import Lexer
import Tokens
import System.Environment
import System.IO

lexr :: String -> IO ()
lexr text = do
    let toks = alexScanTokens text
    mapM_ print toks

main :: IO ()
main = do
    args <- getArgs
    s <- if length args == 0
        then getContents
        else hGetContents =<< (openFile (args!!0) ReadMode)
    lexr s
