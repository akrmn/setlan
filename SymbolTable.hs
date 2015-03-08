module SymbolTable
( Variable(..)
, ScopeType(..)
, SymbolTable(..)
, deepLookup
, varType
) where

import Data.List (intercalate)
import qualified Data.Set as Set (Set(..))
import qualified Data.Map as Map (
    Map(..)
  , delete
  , empty
  , insert
  , lookup
  , member
  , null
  , toList
  )
import Control.Monad (mplus)
import Prelude hiding (lookup)

import AST (Inst(..), Type(..), tabs)

data ScopeType = BlockScope | ForScope deriving (Eq, Show)

varType :: Variable -> Type
varType (IntVar  _ _) = IntType
varType (BoolVar _ _) = BoolType
varType (SetVar  _ _) = SetType
varType (StrVar  _ _) = StrType

data Variable
  = BoolVar { getBool :: Bool
            , scopeType :: ScopeType
            }
  | IntVar  { getInt  :: Int
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
    { variables    :: Map.Map String Variable
    , daughters    :: [SymbolTable]
    , instructions :: [Inst]
    }

instance Show SymbolTable where
  show = show' 0

show' :: Int -> SymbolTable -> String
show' n (SymbolTable variables daughters instructions) =
  tabs n ++ "SCOPE\n" ++ (
    if Map.null variables
      then ""
      else
        tabs (n+1) ++ (
          intercalate
            ("\n" ++ (tabs (n+1)))
            (map show'' (Map.toList variables))
        ) ++ "\n"
  ) ++ (
    concat $ map (show' (n+1)) daughters
  )

show'' :: (String, Variable) -> String
show'' (name, var) =
  name ++ " :: " ++
    case var of
      (BoolVar x _) -> "bool, value = false"
      (IntVar  x _) -> "int, value = 0"
      (SetVar  x _) -> "set, value = {}"
      (StrVar  x _) -> "str, value = \"\""

empty :: SymbolTable
empty =
  SymbolTable
  { variables    = Map.empty
  , daughters    = []
  , instructions = []
  }

insert' :: String -> Variable -> SymbolTable -> SymbolTable
insert' name var st =
  SymbolTable
    (Map.insert name var (variables st)) (daughters st) (instructions st)

insert :: String -> Variable -> SymbolTable -> SymbolTable
insert name var st =
  if name `Map.member` (variables st)
    then error ("Variable " ++ name ++ " already declared in this scope.")
    else insert' name var st

delete :: String -> SymbolTable -> SymbolTable
delete name st =
  SymbolTable (Map.delete name (variables st)) (daughters st) (instructions st)

update :: String -> Variable -> SymbolTable -> SymbolTable
update name var st =
  if name `Map.member` (variables st)
    then insert' name var st
    else error ("Variable " ++ name ++ " not declared in this scope.")

elem :: String -> SymbolTable -> Bool
elem name st =
  name `Map.member` (variables st)

deepLookup :: String -> [SymbolTable] -> Maybe Variable
deepLookup s sts = foldr mplus Nothing $ map (lookup s) sts

lookup :: String -> SymbolTable -> Maybe Variable
lookup name st =
  name `Map.lookup` (variables st)
