module Symbols where

import Parser (parsr)
import AST

scoper :: string -> string -> IO ()
scoper text = do
  putStrLn $ "Symbol Table (" ++ name ++ "):\n"
  --let toks = alexScanTokens text
  --if any isTokenError toks
  --  then mapM_ printError $ filter isTokenError toks
  --  else putStrLn . scoper . parsr $ toks






--scoper :: Inst -> [SymbolTable] -> SymbolTable
checkScope (Assign var value pos) sts
  = if varInTable == Nothing
      then error "Variable " ++ var ++ " not declared in this scope." ++ show pos
      else lType == rType
        where lType = varType (fromJust varInTable)
              rType = expType sts value

  where varInTable = lookup var sts

    lType == rType
  where lType =

checkScope (Block [Declare] [Inst] Pos) sts
  =

checkScope (Scan String Pos) sts
  =

checkScope (Print [Exp] Pos) sts
  =

checkScope (If Exp Inst (Maybe Inst) Pos) sts
  =

checkScope (RWD (Maybe Inst) Exp (Maybe Inst) Pos) sts
  =

checkScope (For String Direction Exp Inst Pos) sts
  =



