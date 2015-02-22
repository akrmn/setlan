module AST where

import Data.List (intersperse)
import Tokens (Pos)

data Program = Program Inst deriving (Eq, Show)

data Inst
  = Assign  String Exp Pos
  | Block   [Declare] [Inst] Pos
  | Scan    String Pos
  | Print   [Exp] Pos
  | If      Exp Inst (Maybe Inst) Pos
  | RWD     (Maybe Inst) Exp (Maybe Inst) Pos
  | For     String Direction Exp Inst Pos
  deriving (Eq, Show)

data Direction = Min | Max deriving (Eq, Show)

data Declare = Declare Type [String] deriving (Eq, Show)

data Type = BoolType | IntType | SetType deriving (Eq)

data Exp
  = Binary     BinOp   Exp  Exp
  | Unary      UnOp    Exp
  | Set        [Exp]
  | IntConst   Int
  | BoolConst  Bool
  | StrConst   String
  | Var        String
  deriving (Eq, Show)

data BinOp
  = Plus     Pos
  | Minus    Pos
  | Times    Pos
  | Div      Pos
  | Mod      Pos
  | SetUnion Pos
  | SetMinus Pos
  | SetInter Pos
  | MapPlus  Pos
  | MapMinus Pos
  | MapTimes Pos
  | MapDiv   Pos
  | MapMod   Pos
  | CompLT   Pos
  | CompLE   Pos
  | CompGT   Pos
  | CompGE   Pos
  | CompEQ   Pos
  | CompNE   Pos
  | CompAt   Pos
  | And      Pos
  | Or       Pos
  deriving (Eq)

data UnOp
  = SetMax   Pos
  | SetMin   Pos
  | SetSize  Pos
  | Not      Pos
  | Negative Pos
  deriving (Eq)

------------------------
-- Printing functions --
------------------------

tabs :: Int -> String
tabs = concat . flip replicate "    "

show' :: Program -> String
show' = show'' 0

class Show'' a where
  show'' :: Int -> a -> String

instance Show'' Program where
  show'' n (Program inst) = "Program\n" ++ (show'' (n+1) inst)

instance Show'' Inst where
  show'' n x = (tabs n) ++
    case x of
      (Assign v e _) ->
        "Assign\n" ++
          (tabs (n+1)) ++ "variable\n" ++
            (tabs (n+2)) ++ v ++ "\n" ++
          (tabs (n+1)) ++ "value\n" ++
            (show'' (n+2) e)

      (Block [] is _) ->
        "Block\n" ++
          (concat $ intersperse "\n" (map (show'' (n+1)) is))

      (Block ds is _) ->
        "Block\n" ++
          (tabs (n+1)) ++ "Using\n" ++
            (concat $ intersperse "\n" (map (show'' (n+2)) ds)) ++ "\n" ++
          (tabs (n+1)) ++ "In\n" ++
          (concat $ intersperse "\n" (map (show'' (n+1)) is))

      (Scan v _) ->
        "Scan\n" ++
          (tabs (n+1)) ++ "variable\n" ++
            (tabs (n+2)) ++ v

      (Print es _) ->
        "Print\n" ++
          (tabs (n+1)) ++ "elements\n" ++
            (concat $ intersperse "\n" (map (show'' (n+2)) es))

      (If c t Nothing _) ->
        "If\n" ++
          (tabs (n+1)) ++ "condition\n" ++
            (show'' (n+2) c) ++ "\n" ++
          (tabs (n+1)) ++ "Then\n" ++
            (show'' (n+2) t)

      (If c t (Just e) _) ->
        "If\n" ++
          (tabs (n+1)) ++ "condition\n" ++
            (show'' (n+2) c) ++ "\n" ++
          (tabs (n+1)) ++ "Then\n" ++
            (show'' (n+2) t) ++ "\n" ++
          (tabs (n+1)) ++"Else\n" ++
            (show'' (n+2) e)

      (RWD Nothing w (Just d) _) ->
        "While\n" ++
          (show'' (n+1) w) ++ "\n" ++
        (tabs n) ++ "Do\n" ++
          (show'' (n+1) d)

      (RWD (Just r) w Nothing _) ->
        "Repeat\n" ++
          (show'' (n+1) r) ++ "\n" ++
        (tabs n) ++ "While\n" ++
          (show'' (n+1) w)

      (RWD (Just r) w (Just d) _) ->
        "Repeat\n" ++
          (show'' (n+1) r) ++ "\n" ++
        (tabs n) ++ "While\n" ++
          (show'' (n+1) w) ++ "\n" ++
        (tabs n) ++ "Do\n" ++
          (show'' (n+1) d)

      (For v d s i _) ->
        "For\n" ++
          (tabs (n+1)) ++ "variable\n" ++
            (tabs (n+2)) ++ v ++ "\n" ++
          (tabs (n+1)) ++ "direction\n" ++
            (tabs (n+2)) ++ show d ++ "\n" ++
          (tabs (n+1)) ++ "in\n" ++
            (show'' (n+2) s) ++ "\n" ++
          (tabs (n+1)) ++ "do\n" ++
            (show'' (n+2) i)

instance Show'' Declare where
  show'' n (Declare t vs) =
    tabs n ++ show t ++ "\n" ++
    (concat $ intersperse "\n" (map (tabs (n+1) ++) vs))

instance Show Type where
  show x =
    case x of
      BoolType -> "bool"
      IntType  -> "int"
      SetType  -> "set"

instance Show'' Exp where
  show'' n x = (tabs n) ++
    case x of
      (Binary op a b) ->
        show op ++ "\n" ++
          (show'' (n+1) a) ++ "\n" ++
          (show'' (n+1) b)

      (Unary  op a) ->
        show op ++ "\n" ++
          (show'' (n+1) a)

      (Set    es) ->
        "Set\n" ++
          (concat $ intersperse "\n" (map (show'' (n+1)) es))

      (IntConst  a) ->
        "Int\n" ++
          (tabs (n+1)) ++ show a

      (BoolConst a) ->
        "Boolean\n" ++
          (tabs (n+1)) ++ show a

      (StrConst  a) ->
        "String\n" ++
          (tabs (n+1)) ++ show a

      (Var       a) ->
        "Variable\n" ++
          (tabs (n+1)) ++ a

instance Show BinOp where
  show x =
    case x of
      (Plus     _) -> "Plus +"
      (Minus    _) -> "Minus -"
      (Times    _) -> "Times *"
      (Div      _) -> "Div /"
      (Mod      _) -> "Mod %"
      (SetUnion _) -> "SetUnion ++"
      (SetMinus _) -> "SetMinus \\"
      (SetInter _) -> "SetInter ><"
      (MapPlus  _) -> "MapPlus <+>"
      (MapMinus _) -> "MapMinus <->"
      (MapTimes _) -> "MapTimes <*>"
      (MapDiv   _) -> "MapDiv </>"
      (MapMod   _) -> "MapMod <%>"
      (CompLT   _) -> "Less Than <"
      (CompLE   _) -> "Less or equal <="
      (CompGT   _) -> "Greater than >"
      (CompGE   _) -> "Greater or equal >="
      (CompEQ   _) -> "Equals =="
      (CompNE   _) -> "Not equal /="
      (CompAt   _) -> "Is Member Of @"
      (And      _) -> "Conjunction And"
      (Or       _) -> "Disjunction Or"

instance Show UnOp where
  show x =
    case x of
      (SetMax   _) -> "SetMax >?"
      (SetMin   _) -> "SetMin <?"
      (SetSize  _) -> "SetSize $?"
      (Not      _) -> "Negation Not"
      (Negative _) -> "Negative (-)"
