module SymbolTable where

import AST (Type(..), tabs, intersperse)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude hiding (lookup)


varType :: Variable -> Type
varType (IntVar  _ _) = IntType
varType (BoolVar _ _) = BoolType
varType (SetVar  _ _) = SetType
varType (StrVar  _ _) = StrType

data ScopeType = BlockScope | ForScope deriving (Eq, Show)

data Variable
  = IntVar  { getInt  :: Int
            , scopeType :: ScopeType
            }
  | BoolVar { getBool :: Bool
            , scopeType :: ScopeType
            }
  | SetVar  { getSet  :: (Set.Set Int)
            , scopeType :: ScopeType
            }
  | StrVar  { getStr  :: String
            , scopeType :: ScopeType
            }
  deriving (Show)

data SymbolTable
  = SymbolTable
    { variables :: Map.Map String Variable
    , daughters :: [SymbolTable]
    }

instance Show SymbolTable where
  show = show' 0

show' :: Int -> SymbolTable -> String
show' n (SymbolTable variables daughters) =
  tabs n ++ "SCOPE\n" ++
    tabs (n+1) ++ (
      concat $
        intersperse
          ("\n" ++ (tabs (n+1)))
          (map show'' (Map.toList variables))
    ) ++ "\n" ++ (
      concat $ map (show' (n+1)) daughters
    )

show'' :: (String, Variable) -> String
show'' (name, var) =
  name ++ " :: " ++
    case var of
      (IntVar  x _) -> "int, value = 0"
      (BoolVar x _) -> "bool, value = false"
      (SetVar  x _) -> "set, value = {}"
      (StrVar  x _) -> "str, value = \"\""

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
