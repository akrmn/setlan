module TypeChecking where

import AST
import Tokens (error')
import SymbolTable

import Data.List (intersperse)
import Data.Maybe (fromJust, isNothing)

mapOp :: [(Type, Type, Type)]
mapOp =  [(IntType, SetType, SetType), (SetType, IntType, SetType)]

intOp :: [(Type, Type, Type)]
intOp =  [(IntType, IntType, IntType )]

setOp :: [(Type, Type, Type)]
setOp =  [(SetType, SetType, SetType)]

relOp :: [(Type, Type, Type)]
relOp =  [(IntType, IntType, BoolType)]

compOp :: [(Type, Type, Type) ]
compOp =  [(SetType,  SetType,  BoolType)
          ,(IntType,  IntType,  BoolType)
          ,(BoolType, BoolType, BoolType)
          ]

atOp :: [(Type, Type, Type)]
atOp =  [(IntType, SetType, BoolType)]

boolOp :: [(Type, Type, Type)]
boolOp =  [( BoolType, BoolType, BoolType)]

binType :: BinOp -> [(Type, Type, Type)]
binType (Plus     _) = intOp
binType (Minus    _) = intOp
binType (Times    _) = intOp
binType (Div      _) = intOp
binType (Mod      _) = intOp
binType (SetUnion _) = setOp
binType (SetMinus _) = setOp
binType (SetInter _) = setOp
binType (MapPlus  _) = mapOp
binType (MapMinus _) = mapOp
binType (MapTimes _) = mapOp
binType (MapDiv   _) = mapOp
binType (MapMod   _) = mapOp
binType (CompLT   _) = relOp
binType (CompLE   _) = relOp
binType (CompGT   _) = relOp
binType (CompGE   _) = relOp
binType (CompEQ   _) = compOp
binType (CompNE   _) = compOp
binType (CompAt   _) = atOp
binType (And      _) = boolOp
binType (Or       _) = boolOp

binCheck :: Type -> Type -> (Type, Type, Type) -> [Type]
binCheck a b (c, d, e) =
  if a == c && b == d
    then [e]
  else []

unType :: UnOp -> [(Type, Type)]
unType (SetMax   _) = [(SetType, IntType)]
unType (SetMin   _) = [(SetType, IntType)]
unType (SetSize  _) = [(SetType, IntType)]
unType (Not      _) = [(BoolType, BoolType)]
unType (Negative _) = [(IntType, IntType)]

unCheck :: Type -> (Type, Type) -> [Type]
unCheck a (b, c) =
  if a == b
    then [c]
  else []

expType :: [SymbolTable] -> Exp -> Type
expType sts (Binary binOp exp0 exp1) =
  if null matches
    then error' pos (
      "Operator " ++ show binOp ++ " expects\n    " ++ (
        concat $ intersperse " or\n    " (map show' expected)
      ) ++
      "\nbut received\n    " ++ show type0 ++ ", " ++ show type1
      )
    else head matches
  where
    matches         = (binCheck type0 type1) =<< expected
    type0           = expType sts exp0
    type1           = expType sts exp1
    expected        = binType binOp
    show' (x, y, _) = show x ++ ", " ++ show y
    pos             = getPosB binOp


expType sts (Unary unOp exp0) =
  if null matches
    then error' pos (
      "Operator " ++ show unOp ++ "expects\n    " ++
      show expected ++ "\nbut received\n    " ++ show type0
      )
    else head matches
  where
    matches  = (unCheck type0) =<< expected
    type0    = expType sts exp0
    expected = unType unOp
    pos      = getPosU unOp

expType sts (Set exps pos) =
  if all (\x -> expType sts x == IntType) exps
    then SetType
    else error' pos (
      "Set can only have integer values"
    )

expType _ (IntConst _)  = IntType

expType _ (BoolConst _) = BoolType

expType _ (StrConst _)  = StrType

expType sts (Var (Id var pos))   =
  if isNothing varDef
    then error' pos (
      "Variable `" ++ var ++ "` not declared in this scope."
    )
    else varType $ fromJust varDef
  where varDef = deepLookup var sts
