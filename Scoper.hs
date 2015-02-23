module Scoper where

import Data.Maybe (isNothing, isJust, fromJust)

import Parser (parsr)
import Tokens (error')
import AST



--scoper :: String -> String -> IO ()
--scoper text = do
--  putStrLn $ "Symbol Table (" ++ name ++ "):\n"
--  --let toks = alexScanTokens text
--  --if any isTokenError toks
--  --  then mapM_ printError $ filter isTokenError toks
--  --  else putStrLn . scoper . parsr $ toks







scoper :: [SymbolTable] -> Inst -> [SymbolTable]
scoper sts (Assign var value pos) =
  if isNothing varDef
    then error' pos (
      "Variable " ++ var ++ " not declared in this scope."
    )
    else if scopeType == ForScope
      then error' pos (
        "Cannot reassign iteration variable."
      )
      else if lType /= rType
        then error' pos (
          "Variable " ++ var ++ "::" ++ show lType ++
          " cannot receive " ++ show rType " expression in assignment."
        )
        else []
  where varDef    = deepLookup var sts
        scopeType = scopeType (fromJust varDef)
        lType     = varType (fromJust varDef)
        rType     = expType sts value

scoper sts (Block [Declare] [Inst] Pos) =

scoper sts (Scan var pos) =
  if varDef == Nothing
    then error pos (
      "Variable " ++ var ++ " not declared in this scope."
    )
    else []

scoper sts (Print exps pos) =
  seq (map (expType sts) exps) []

scoper sts (If cond then_ else_ pos) =
  if condType /= BoolType
      then error' pos (
        "If instruction expects bool expression, not " ++
        show condType
      )
      else scoper then_ sts ++
        if isJust else_
          then scoper (fromJust else_) sts
          else []
    where condType = expType sts cond

scoper sts (RWD r cond d pos) =
  (
    if isJust r
      then scoper (fromJust r) sts
      else []
  ) ++ (
    if condType /= BoolType
      then error' pos (
        "While instruction expects bool expression, not " ++
        show condType
      )
      else []
    where condType = expType sts cond
  ) ++ (
    if isJust d
      then scoper (fromJust d) sts
      else []
  )

scoper sts (For name _ inst pos) =
  if rangeType /= SetType
    then error' pos (
      "For instruction expects set expression, not " ++
      show rangeType
    )
    else scoper sts' inst
  where rangeType = expType range
        sts'      = newST : sts
        newST     = insert name var empty
        var       = IntVar 0 ForScope
