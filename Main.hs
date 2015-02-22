module Main (main) where

import Flags
import Lexer (lexer)
import Parser (parser)
import System.Environment
import System.IO
import Control.Monad (liftM, mapM)

context :: String -> String -> IO ()
context text name = do
    putStrLn $ "Context (" ++ name ++ "):\n"
    putStrLn "ajndkjandk"
    putStrLn ""

showHelp = do
    putStrLn "setlan usage:"
    putStrLn "    setlan <file0> .. <fileN> <flag0> .. <flagM>"
    putStrLn "    the flags can be any combination of:"
    putStrLn "        -l or -t : lexer (tokens)"
    putStrLn "        -p or -a : parser (abstract syntax tree)"
    putStrLn "        -c or -s : context tree (symbol table)"
    putStrLn "        any other: this infomation is shown"
    putStrLn "    if no flags are given, the default command is run:"
    putStrLn "        context tree"
    putStrLn "    if no files are given, the program reads from stdin"

run :: Flag -> (String -> String -> IO ())
run DoLex     = lexer
run DoParse   = parser
run DoContext = context

main :: IO ()
main = do
    flags <- (liftM takeFlags) getArgs
    let flags' = if null flags
        then [DoContext]
        else flags

    files <- (liftM takeFiles) getArgs
    contents <- if not (null files)
        then (mapM readFile) files
        else (mapM id) [getContents]

    let files' = if null files
        then ["stdin"]
        else files

    if WrongFlag `elem` flags
        then showHelp
        else sequence_ [run f x y | (x,y) <- zip contents files', f <- flags']
