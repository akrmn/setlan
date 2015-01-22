module Main (main) where

import Lexer
import Tokens
import System.Environment
import System.IO

fromFile :: FilePath -> IO()
fromFile filename = do
    handle <- openFile (filename) ReadMode
    s <- hGetContents handle
    let toks = alexScanTokens s
    mapM_ print toks

main :: IO ()
main = do
    let s = ""
    args <- getArgs
    if length args == 0
        then do
            s <- getContents
            let toks = alexScanTokens s
            mapM_ print toks
        else do
            mapM_ fromFile args
