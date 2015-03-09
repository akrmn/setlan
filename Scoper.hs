module Scoper
( scoper
, scoper''
) where

import Data.Maybe (
    fromJust
  , isJust
  , isNothing
  )
import Data.List (intercalate)
import qualified Data.Set as Set (empty)
import qualified Data.Map as Map (
    Map(..)
  , empty
  , insert
  , member
  , singleton
  )

import Tokens (isTokenError, printError)
import Lexer (alexScanTokens)
import Parser (parsr)
import TypeChecking (expType)
import SymbolTable (
    Variable(..)
  , SymbolTable(..)
  , ScopeType(..)
  , varType
  , deepLookup
  )
import AST (
    Program(..)
  , Inst(..)
  , Declare(..)
  , Type(..)
  , Id(..)
  )

scoper :: String -> String -> IO ()
scoper text name = do
  putStrLn $ "Symbol Table (" ++ name ++ "):\n"
  let toks = alexScanTokens text
  if any isTokenError toks
    then mapM_ printError $ filter isTokenError toks
    else
      let scope = scoper'' . parsr $ toks
      in case scope of
        Left error -> putStr error
        Right st   -> putStr (show st)
  putStrLn ""

scoper'' :: Program -> Either String SymbolTable
scoper'' (Program inst) =
  if null errors
    then if (length scopes > 1)
      then Left "WTF" -- shouldn't ever get here
      else Right programST
   else Left $ "ERROR: " ++ (intercalate "\nERROR: " errors)
  where
    (scopes, errors) = scoper' [] inst
    programST =
      SymbolTable
        { scopeType    = ProgramScope
        , variables    = Map.empty
        , daughters    = scopes
        , instructions = [inst]
        }

type VarMap = Map.Map String Variable

getVars :: [Declare] -> (VarMap, [String])
getVars = getVars' Map.empty

getVars' :: VarMap -> [Declare] -> (VarMap, [String])
getVars' vars [] =
  (vars, [])
getVars' vars ((Declare t (Id name pos)) : xs) =
  if name `Map.member` vars
    then let
      (vars', errors) =
        getVars' vars xs
      errorAlreadyDeclared =
        show pos ++ " Variable `" ++ name ++
        "` already declared in this scope."
      in (vars', errorAlreadyDeclared : errors)
    else let
      (vars'', errors) = getVars' vars' xs
      vars' = Map.insert name (
        case t of
          BoolType -> BoolVar False
          IntType  -> IntVar  0
          SetType  -> SetVar  Set.empty
          StrType  -> StrVar  ""
        ) vars
      in (vars'', errors)

scoper' :: [SymbolTable] -> Inst -> ([SymbolTable], [String])

-- Assign Instruction --
scoper' sts (Assign (Id name _) value pos) =
  ([], errors)
  where
    varDef = deepLookup name sts
    scope  = snd (fromJust varDef)
    lType  = varType (fst $ fromJust varDef)
    rType  = expType sts value
    errors =
      if isNothing varDef
        then [errorUndeclared]
        else if scope == ForScope
          then [errorAssignIteration]
          else case rType of
            TypeError es ->
              es
            _ ->
              if lType /= rType
                then [errorTypeMismatch]
                else []
    errorUndeclared =
      show pos ++ " Variable `" ++ name ++ "` not declared in this scope."
    errorAssignIteration =
      show pos ++ " Cannot reassign iteration variable `" ++ name ++ "`."
    errorTypeMismatch =
      show pos ++ " Variable `" ++ name ++ "` of type `" ++ show lType ++
      "` cannot receive `" ++ show rType ++ "` type expression in assignment."


-- Block Instruction --
scoper' sts (Block declares insts pos) =
  ( [blockScope]
  , varErrors ++ daughterErrors
  )
  where
    blockScope = SymbolTable
      { scopeType    = BlockScope
      , variables    = vars
      , daughters    = daughterScopes
      , instructions = insts
      }
    (daughterScopes, daughterErrors) =
      (\(x, y) -> (concat x, concat y)) $ unzip (map (scoper' sts') insts)
    sts' = blockScope : sts
    (vars, varErrors) = getVars declares


-- Scan Instruction --
scoper' sts (Scan (Id name _) pos) =
  ([], errors)
  where
    errors = case varDef of
      Nothing -> [errorUndeclared]
      Just (x,_) -> case varType x of
        SetType -> [errorScanSet]
        _ -> []
    varDef = deepLookup name sts
    errorUndeclared =
      show pos ++ " Variable `" ++ name ++ "` not declared in this scope."
    errorScanSet =
      show pos ++ " Can't scan variable `" ++ name ++ "` of type `set`."


-- Print Instruction --
scoper' sts (Print exps pos) =
  ([], errors)
  where
    errors = concat [ s | TypeError s <- types ]
    types = map (expType sts) exps


-- If Instruction --
scoper' sts (If cond thn els pos) =
  ( [ifScope]
  , ifErrors ++ thnErrors ++ elsErrors
  )
  where
    ifScope = SymbolTable
      { scopeType    = IfScope
      , variables    = Map.empty
      , daughters    = thnScopes ++ elsScopes
      , instructions = []
      }
    (thnScopes, thnErrors) = scoper' sts thn
    (elsScopes, elsErrors) =
      case els of
        Just x ->
          scoper' sts x
        Nothing ->
          ([],[])
    ifErrors =
      case condType of
        TypeError es ->
          es
        _ ->
          if condType /= BoolType
            then [errorNonBoolCondition]
            else []
    errorNonBoolCondition =
      show pos ++ " `If` instruction expects `bool` type expression, not `" ++
      show condType ++ "`."
    condType = expType sts cond


-- RWD Instruction --
scoper' sts (RWD r cond d pos) =
  ( [whileScope]
  , rErrors ++ wErrors ++ dErrors
  )
  where
    whileScope = SymbolTable
      { scopeType    = WhileScope
      , variables    = Map.empty
      , daughters    = rScopes ++ dScopes
      , instructions = []
      }
    (rScopes, rErrors) =
      case r of
        Just r' -> scoper' sts r'
        Nothing -> ([],[])
    (dScopes, dErrors) =
      case d of
        Just d' -> scoper' sts d'
        Nothing -> ([],[])
    wErrors =
      case condType of
        TypeError es ->
          es ++ [errorNonBoolCondition]
        _ ->
          if condType /= BoolType
            then [errorNonBoolCondition]
            else []
    errorNonBoolCondition =
      show pos ++ " `While` instruction expects `bool` type expression, not `" ++
      show condType ++ "`."
    condType = expType sts cond


-- For Instruction --
scoper' sts (For (Id name _) _ range inst pos) =
  ( [forScope]
  , forErrors ++ daughterErrors
  )
  where
    forScope = SymbolTable
      { scopeType    = ForScope
      , variables    = Map.singleton name (IntVar 0)
      , daughters    = daughterScopes
      , instructions = [inst]
      }
    sts' = forScope : sts
    (daughterScopes, daughterErrors) = scoper' sts' inst
    forErrors =
      case rangeType of
        TypeError es ->
          es
        _ ->
          if rangeType /= SetType
            then [errorNonSetRange]
            else []
    errorNonSetRange =
      show pos ++ " `For` instruction expects `set` type expression, not `" ++
      show rangeType ++ "`."
    rangeType = expType sts range
