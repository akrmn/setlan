module Flags where

import Data.List (nub, sort)
import System.Environment
import System.IO

data Flag
  = DoLex
  | DoParse
  | DoContext
  | Help
  | WrongFlag
  deriving (Eq, Show, Ord)

toFlag :: Char -> Flag
toFlag 't' = DoLex
toFlag 'l' = DoLex

toFlag 'a' = DoParse
toFlag 'p' = DoParse

toFlag 's' = DoContext
toFlag 'c' = DoContext

toFlag  x  = WrongFlag

toFlags :: String -> [Flag]
toFlags []     = []
toFlags (x:xs) = toFlag x : toFlags xs

takeFlags :: [String] -> [Flag]
takeFlags x = sort (nub flags)
  where flags = x' >>= toFlags
        x'    = map tail (filter (\x -> head x == '-') x)

takeFiles :: [String] -> [String]
takeFiles = filter (\x -> head x /= '-')
