module Main
( main
) where

import System.Environment (getArgs)
import Control.Monad (liftM, mapM)

import Flags (
    Flag(..)
  , takeFlags
  , takeFiles
  )
import Lexer (lexer)
import Parser (parser)
import Scoper (scoper)
import Interpreter (interpreter)

help :: String
help = unlines
  [ "setlan usage:"
  , "    setlan <flag0> .. <flagM> <file0> .. <fileN>"
  , "    the flags can be any combination of:"
  , "        -l or -t : lexer (tokens)"
  , "        -p or -a : parser (abstract syntax tree)"
  , "        -s or -c : scoper (symbol table, context)"
  , "        -i or -e : interpreter (execute)"
  , "        any other: this infomation is shown"
  , "    if no flags are given, the default command is executed:"
  , "        interpreter"
  , "    if no files are given, the program reads from stdin"
  ]

run :: Flag -> (String -> String -> IO ())
run DoLex       = lexer
run DoParse     = parser
run DoScope     = scoper
run DoInterpret = interpreter

main :: IO ()
main = do
  flags <- liftM takeFlags getArgs
  let
    flags' = if null flags
      then [DoInterpret]
      else flags

  files <- liftM takeFiles getArgs
  contents <- if not (null files)
    then mapM readFile files
    else mapM id [getContents]

  let
    files' = if null files
      then ["stdin"]
      else files

  if (WrongFlag `elem` flags) || ((null flags) && (null files))
    then putStr help
    else sequence_ [run f x y | (x,y) <- zip contents files', f <- flags']
