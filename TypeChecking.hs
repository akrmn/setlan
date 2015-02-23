module TypeChecking where

import Parser (tp)
import AST hiding (show')
import Tokens (error')
import Data.List (intersperse)
import Data.Maybe (fromJust, isNothing)

mapOp :: [(Type, Type, Type)]
mapOp =  [(IntType, SetType, SetType), (SetType, IntType, SetType)]

intOp :: [(Type, Type, Type)]
intOp =  [(IntType, IntType, IntType )]

setOp :: [(Type, Type, Type)]
setOp =  [(SetType, SetType, SetType)]

relOp :: [(Type, Type, Type)]
compintOp =  [(IntType, IntType, BoolType)]

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
binType (CompLT   _) = compintOp
binType (CompLE   _) = compintOp
binType (CompGT   _) = compintOp
binType (CompGE   _) = compintOp
binType (CompEQ   _) = compOp
binType (CompNE   _) = compOp
binType (CompAt   _) = atOp
binType (And      _) = boolOp
binType (Or       _) = boolOp

binCheck :: Type -> Type -> (Type, Type, Type) -> [Type]
binCheck a b (c, d, e) =
  if a == d && b == e
    then [e]
  else []

unType :: UnOp -> [(Type, Type)]
UnType (SetMax   _) = [(SetType, IntType)]
UnType (SetMin   _) = [(SetType, IntType)]
UnType (SetSize  _) = [(SetType, IntType)]
UnType (Not      _) = [(BoolType, BoolType)]
UnType (Negative _) = [(IntType, IntType)]

unCheck :: Type -> (Type, Type) -> [Type]
unCheck a (b, c) =
  if a == b
    then [c]
  else []

expType :: [SymbolTable] -> Exp -> Type
expType sts (Binary binOp exp0 exp1) =
  if null matches
    then error' pos (
      "Operator " ++ show binOp ++ "expects\n    " ++
      concat intersperse " or\n    " (map show' expected) ++
      "but received\n    " ++ show type0 ++ ", " ++ show type1
      )
    else head matches
  where matches         = (binCheck type0 type1) =<< expected
        type0           = expType sts exp0
        type1           = expType sts exp1
        expected        = binType binOp
        show' (x, y, _) = show x ++ ", " ++ show y
        pos             = tp binOp


expType sts (Unary unOp exp) =
  if null matches
    then error' pos (
      "Operator " ++ show unOp ++ "expects\n    " ++
      show expected ++ "but received\n    " ++ show expType exp0
      )
    else head matches
  where matches  = (unCheck expType exp) =<< expected
        expected = unType unOp
        pos      = tp unOp

expType sts (Set exps pos) =
  if all (\x -> expType sts x == IntType)
    then SetType
    else error' pos(
      "Set can only have integer values"
    )

expType _ (IntConst _)  = IntType

expType _ (BoolConst _) = BoolType

expType _ (StrConst _)  = StrType

expType sts (Var var)   =
  if isNothing varDef
    then error' pos (
      "Variable " ++ var ++ " not declared in this scope."
    )
    else varType $ fromJust varDef
  where varDef = deepLookup var sts

varType :: Variable -> Type
varType (IntVar  _ _) = IntType
varType (BoolVar _ _) = BoolType
varType (SetVar  _ _) = SetType
varType (StrVar  _ _) = StrType
