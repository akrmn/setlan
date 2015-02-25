module Flags where

import Data.List (nub, sort)
import System.Environment
import System.IO

data Flag
  = WrongFlag
  | DoLex
  | DoParse
  | DoScope
  deriving (Eq, Show, Ord)

toFlag :: Char -> Flag
toFlag 't' = DoLex
toFlag 'l' = DoLex

toFlag 'a' = DoParse
toFlag 'p' = DoParse

toFlag 's' = DoScope
toFlag 'c' = DoScope

toFlag  _  = WrongFlag

takeFlags :: [String] -> [Flag]
takeFlags x = sort (nub flags)
  where flags = x' >>= (map toFlag)
        x'    = map tail (filter (\x -> head x == '-') x)

takeFiles :: [String] -> [String]
takeFiles = filter (\x -> head x /= '-')
