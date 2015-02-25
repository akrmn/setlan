module Main (main) where

import Flags
import Lexer  (lexer)
import Parser (parser)
import Scoper (scoper)
import System.Environment
import System.IO
import Control.Monad (liftM, mapM)

help :: String
help = unlines
    [ "setlan usage:"
    , "    setlan <file0> .. <fileN> <flag0> .. <flagM>"
    , "    the flags can be any combination of:"
    , "        -l or -t : lexer (tokens)"
    , "        -p or -a : parser (abstract syntax tree)"
    , "        -c or -s : context tree (symbol table)"
    , "        any other: this infomation is shown"
    , "    if no flags are given, the default command is run:"
    , "        context tree"
    , "    if no files are given, the program reads from stdin"
    ]

run :: Flag -> (String -> String -> IO ())
run DoLex   = lexer
run DoParse = parser
run DoScope = scoper

main :: IO ()
main = do
    flags <- liftM takeFlags getArgs
    let flags' = if null flags
        then [DoScope]
        else flags

    files <- liftM takeFiles getArgs
    contents <- if not (null files)
        then mapM readFile files
        else mapM id [getContents]

    let files' = if null files
        then ["stdin"]
        else files

    if WrongFlag `elem` flags
        then putStr help
        else sequence_ [run f x y | (x,y) <- zip contents files', f <- flags']
