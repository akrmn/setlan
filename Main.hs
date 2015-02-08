module Main (main) where

import Parser (parser)
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    source <- if length args == 0
        then getContents
        else hGetContents =<< (openFile (args!!0) ReadMode)
    parser source
