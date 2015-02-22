module SymbolTable where

import AST (Type(..))

import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude hiding (lookup)

varType :: Variable -> Type
varType (IntVar  _) = IntType
varType (BoolVar _) = BoolType
varType (SetVar  _) = SetType

data Variable
  = IntVar  {getInt  :: Int}
  | BoolVar {getBool :: Bool}
  | SetVar  {getSet  :: (Set.Set Int)}
  deriving (Show)

data SymbolTable
  = SymbolTable
  { variables :: Map.Map String Variable
  , daughters :: [SymbolTable]
  }
  deriving (Show)

empty :: SymbolTable
empty =
  SymbolTable
  { variables = Map.empty
  , daughters = []
  }

insert' :: String -> Variable -> SymbolTable -> SymbolTable
insert' name var st =
  SymbolTable (Map.insert name var (variables st)) (daughters st)

insert :: String -> Variable -> SymbolTable -> SymbolTable
insert name var st =
  if name `Map.member` (variables st)
    then error ("Variable " ++ name ++ " already declared in this scope.")
    else insert' name var st

delete :: String -> SymbolTable -> SymbolTable
delete name st =
  SymbolTable (Map.delete name (variables st)) (daughters st)

update :: String -> Variable -> SymbolTable -> SymbolTable
update name var st =
  if name `Map.member` (variables st)
    then insert' name var st
    else error ("Variable " ++ name ++ " not declared in this scope.")

elem :: String -> SymbolTable -> Bool
elem name st =
  name `Map.member` (variables st)

firstJust :: [Maybe a] -> Maybe a
firstJust []             = Nothing
firstJust (Just x : xs)  = Just x
firstJust (Nothing : xs) = firstJust xs

deepLookup :: String -> [SymbolTable] -> Maybe Variable
deepLookup s sts = firstJust $ map (lookup s) sts

lookup :: String -> SymbolTable -> Maybe Variable
lookup name st =
  name `Map.lookup` (variables st)
