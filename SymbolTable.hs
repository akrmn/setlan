module SymbolTable
( Variable(..)
, ScopeType(..)
, SymbolTable(..)
, varType
, empty
, insert
, delete
, deepUpdate
, update
, SymbolTable.elem
, deepLookup
, lookup
) where

import Data.List (intercalate)
import qualified Data.Set as Set (Set(..), toAscList)
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

data ScopeType
  = ProgramScope
  | WhileScope
  | IfScope
  | BlockScope
  | ForScope
  deriving (Eq)

instance Show ScopeType where
  show x = case x of
    ProgramScope -> "Program"
    WhileScope   -> "While"
    IfScope      -> "If"
    BlockScope   -> "Block"
    ForScope     -> "For"

varType :: Variable -> Type
varType (IntVar  _) = IntType
varType (BoolVar _) = BoolType
varType (SetVar  _) = SetType
varType (StrVar  _) = StrType

data Variable
  = BoolVar { getBool :: Bool }
  | IntVar  { getInt  :: Int }
  | SetVar  { getSet  :: (Set.Set Int) }
  | StrVar  { getStr  :: String }
  deriving (Eq, Ord)

instance Show Variable where
  show (BoolVar True)  = "true"
  show (BoolVar False) = "false"

  show (IntVar i) = show i

  show (SetVar elems) =
    "{" ++ (intercalate ", " $ map show (Set.toAscList elems)) ++ "}"

  show (StrVar s) = s

data SymbolTable
  = SymbolTable
    { scopeType    :: ScopeType
    , variables    :: Map.Map String Variable
    , daughters    :: [SymbolTable]
    , instructions :: [Inst]
    }

instance Show SymbolTable where
  show = show' 0

show' :: Int -> SymbolTable -> String
show' n (SymbolTable scopeType variables daughters instructions) =
  tabs n ++ "SCOPE (" ++ show scopeType ++ ")\n" ++ (
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
  name ++ " :: " ++ (
    case var of
      (BoolVar x) -> "bool"
      (IntVar  x) -> "int"
      (SetVar  x) -> "set"
      (StrVar  x) -> "str"
  ) ++ ", value = " ++ show var

empty :: SymbolTable
empty =
  SymbolTable
  { scopeType    = ProgramScope
  , variables    = Map.empty
  , daughters    = []
  , instructions = []
  }

insert' :: String -> Variable -> SymbolTable -> SymbolTable
insert' name var (SymbolTable scopeType variables daughters instructions) =
  SymbolTable
    scopeType
    (Map.insert name var (variables))
    daughters
    instructions

insert :: String -> Variable -> SymbolTable -> SymbolTable
insert name var st =
  if name `Map.member` (variables st)
    then error ("Variable " ++ name ++ " already declared in this scope.")
    else insert' name var st

delete :: String -> SymbolTable -> SymbolTable
delete name (SymbolTable scopeType variables daughters instructions) =
  SymbolTable
    scopeType
    (Map.delete name (variables))
    daughters
    instructions

deepUpdate :: String -> Variable -> [SymbolTable] -> [SymbolTable]
deepUpdate name var [] =
  []
deepUpdate name var (st:sts) =
  if name `Map.member` (variables st)
    then (insert' name var st):sts
    else st:(deepUpdate name var sts)

update :: String -> Variable -> SymbolTable -> SymbolTable
update name var st =
  if name `Map.member` (variables st)
    then insert' name var st
    else error ("Variable " ++ name ++ " not declared in this scope.")

elem :: String -> SymbolTable -> Bool
elem name st =
  name `Map.member` (variables st)

deepLookup :: String -> [SymbolTable] -> Maybe (Variable, ScopeType)
deepLookup s sts =
  foldr mplus Nothing $ map (lookup s) sts

lookup :: String -> SymbolTable -> Maybe (Variable, ScopeType)
lookup name st =
  case name `Map.lookup` (variables st) of
    Just x  -> Just (x, scopeType st)
    Nothing -> Nothing
