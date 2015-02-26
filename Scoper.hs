module Scoper (scoper) where

import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Safe (headMay)

import Lexer (alexScanTokens, printError)
import Parser (parsr)
import Tokens (error', isTokenError, Pos)
import TypeChecking
import SymbolTable
import AST

data ScopeError = ScopeError Pos String

scoper :: String -> String -> IO ()
scoper text name = do
  putStrLn $ "Symbol Table (" ++ name ++ "):\n"
  let toks = alexScanTokens text
  if any isTokenError toks
    then mapM_ printError $ filter isTokenError toks
    else putStr . scoper'' . parsr $ toks
  putStrLn ""

scoper'' :: Program -> String
scoper'' (Program inst) =
  if null (fst scopes)
    then "No Scopes"
    else show $ (head (fst scopes))
  where
    scopes = scoper' [] inst

getVars :: Map.Map String Variable -> [Declare] -> Map.Map String Variable
getVars vars []                   = vars
getVars vars ((Declare t ids):xs) = getVars vars' xs
  where
    vars' = getVars' t vars ids

getVars' :: Type -> Map.Map String Variable -> [Id] -> Map.Map String Variable
getVars' t vars []                 = vars
getVars' t vars ((Id name pos):xs) = getVars' t vars' xs
  where
    vars' =
      if name `Map.member` vars
        then seq err vars
        else Map.insert name (
          case t of
            BoolType -> BoolVar False     BlockScope
            IntType  -> IntVar  0         BlockScope
            SetType  -> SetVar  Set.empty BlockScope
            StrType  -> StrVar  ""        BlockScope
          ) vars
    err =
      error' pos (
        "Variable `" ++ name ++ "` already declared in this scope."
      )

scoper' :: [SymbolTable] -> Inst -> ([SymbolTable], [ScopeError])
scoper' sts (Assign (Id name _) value pos) =
  if isNothing varDef
    then ([], [ScopeError pos (
      "Variable `" ++ name ++ "` not declared in this scope."
      )]
    )
    else if scope == ForScope
      then ([], [ScopeError pos (
        "Cannot reassign iteration variable."
        )]
      )
      else if lType /= rType
        then ([], [ScopeError pos (
          "Variable " ++ name ++ "::" ++ show lType ++
          " cannot receive " ++ show rType ++ " expression in assignment."
          )]
        )
        else ([],[])
  where
    varDef = deepLookup name sts
    scope  = scopeType (fromJust varDef)
    lType  = varType (fromJust varDef)
    rType  = expType sts value

scoper' sts (Block declares insts pos) =
  if null declares
    then ([], [])
    else ([newST], [])
  where
    newST = SymbolTable
      { variables = getVars Map.empty declares
      , daughters = (map fst ((scoper' sts') =<< insts))
      }
    sts' = newST : sts

scoper' sts (Scan (Id name pos) _) =
  if isNothing varDef
    then ([], [ScopeError pos (
      "Variable `" ++ name ++ "` not declared in this scope."
      )]
    )
    else ([], [])
  where
    varDef = deepLookup name sts

scoper' sts (Print exps pos) =
  seq (map (expType sts) exps) []

scoper' sts (If cond thn els pos) =
  if condType /= BoolType
    then ([], [ScopeError pos (
      "`If` instruction expects bool expression, not " ++ show condType
      )]
    )
    else scoper' sts thn  ++ (
      if isJust els
        then scoper' sts (fromJust els)
        else ([], [])
    )
  where condType = expType sts cond

scoper' sts (RWD r cond d pos) =
  (
    if isJust r
      then scoper' sts (fromJust r)
      else ([], [])
  ) ++ (
    if condType /= BoolType
      then ([], [ScopeError pos (
        "`While` instruction expects bool expression, not " ++ show condType
      )])
      else ([], [])
  ) ++ (
    if isJust d
      then scoper' sts (fromJust d)
      else ([], [])
  )
    where condType = expType sts cond

scoper' sts (For (Id name _) _ range inst pos) =
  if rangeType /= SetType
    then ([], [ScopeError pos (
      "`For` instruction expects set expression, not " ++ show rangeType
    )])
    else ([newST], [])
  where
    rangeType = expType sts range
    newST     = SymbolTable
      { variables = Map.insert name (IntVar 0 ForScope) Map.empty
      , daughters = scoper' sts' inst
      }
    sts'      = newST : sts
