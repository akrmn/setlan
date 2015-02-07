-- CI3725 Traductores e interpretadores
-- Project part 1 - Lexer
-- Members:
--         Moises Ackerman 11-10005
--         Carlos Ferreira 11-10323

module Main (main) where

import Lexer
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    s <- if length args == 0
        then getContents
        else hGetContents =<< (openFile (args!!0) ReadMode)
    lexer s
