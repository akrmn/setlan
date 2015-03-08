module AST
( Program(..)
, Inst(..)
, Direction(..)
, Declare(..)
, Type(..)
, Id(..)
, Exp(..)
, BinOp(..)
, UnOp(..)
, tabs
) where

import Data.List (intercalate)

import Tokens (Pos)

data Program = Program Inst deriving (Eq)

data Inst
  = Assign  Id Exp Pos
  | Block   [Declare] [Inst] Pos
  | Scan    Id Pos
  | Print   [Exp] Pos
  | If      Exp Inst (Maybe Inst) Pos
  | RWD     (Maybe Inst) Exp (Maybe Inst) Pos
  | For     Id Direction Exp Inst Pos
  deriving (Eq, Show)

data Direction = Min | Max deriving (Eq, Show)

data Declare = Declare Type Id deriving (Eq, Show)

data Type
  = BoolType
  | IntType
  | SetType
  | StrType
  | TypeError [String]
  deriving (Eq)

data Id = Id String Pos deriving (Eq)

data Exp
  = Binary     BinOp   Exp  Exp
  | Unary      UnOp    Exp
  | Set        [Exp]   Pos
  | BoolConst  Bool
  | IntConst   Int
  | StrConst   String
  | Var        Id
  deriving (Eq, Show)

data BinOp
  = Plus     {getPosB :: Pos}
  | Minus    {getPosB :: Pos}
  | Times    {getPosB :: Pos}
  | Div      {getPosB :: Pos}
  | Mod      {getPosB :: Pos}
  | SetUnion {getPosB :: Pos}
  | SetMinus {getPosB :: Pos}
  | SetInter {getPosB :: Pos}
  | MapPlus  {getPosB :: Pos}
  | MapMinus {getPosB :: Pos}
  | MapTimes {getPosB :: Pos}
  | MapDiv   {getPosB :: Pos}
  | MapMod   {getPosB :: Pos}
  | CompLT   {getPosB :: Pos}
  | CompLE   {getPosB :: Pos}
  | CompGT   {getPosB :: Pos}
  | CompGE   {getPosB :: Pos}
  | CompEQ   {getPosB :: Pos}
  | CompNE   {getPosB :: Pos}
  | CompAt   {getPosB :: Pos}
  | And      {getPosB :: Pos}
  | Or       {getPosB :: Pos}
  deriving (Eq)

data UnOp
  = SetMax   {getPosU :: Pos}
  | SetMin   {getPosU :: Pos}
  | SetSize  {getPosU :: Pos}
  | Not      {getPosU :: Pos}
  | Negative {getPosU :: Pos}
  deriving (Eq)

------------------------
-- Printing functions --
------------------------

tabs :: Int -> String
tabs = concat . flip replicate "    "

instance Show Program where
  show = show' 0

class Show' a where
  show' :: Int -> a -> String

instance Show' Program where
  show' n (Program inst) = "Program\n" ++ (show' (n+1) inst)

instance Show' Inst where
  show' n x = (tabs n) ++
    case x of
      (Assign (Id v _) e _) ->
        "Assign\n" ++
          (tabs (n+1)) ++ "variable\n" ++
            (tabs (n+2)) ++ v ++ "\n" ++
          (tabs (n+1)) ++ "value\n" ++
            (show' (n+2) e)

      (Block [] is _) ->
        "Block\n" ++
          (intercalate "\n" (map (show' (n+1)) is))

      (Block ds is _) ->
        "Block\n" ++
          (tabs (n+1)) ++ "Using\n" ++
            (intercalate "\n" (map (show' (n+2)) ds)) ++ "\n" ++
          (tabs (n+1)) ++ "In\n" ++
          (intercalate "\n" (map (show' (n+1)) is))

      (Scan (Id v _) _) ->
        "Scan\n" ++
          (tabs (n+1)) ++ "variable\n" ++
            (tabs (n+2)) ++ v

      (Print es _) ->
        "Print\n" ++
          (tabs (n+1)) ++ "elements\n" ++
            (intercalate "\n" (map (show' (n+2)) es))

      (If c t Nothing _) ->
        "If\n" ++
          (tabs (n+1)) ++ "condition\n" ++
            (show' (n+2) c) ++ "\n" ++
          (tabs (n+1)) ++ "Then\n" ++
            (show' (n+2) t)

      (If c t (Just e) _) ->
        "If\n" ++
          (tabs (n+1)) ++ "condition\n" ++
            (show' (n+2) c) ++ "\n" ++
          (tabs (n+1)) ++ "Then\n" ++
            (show' (n+2) t) ++ "\n" ++
          (tabs (n+1)) ++"Else\n" ++
            (show' (n+2) e)

      (RWD Nothing w (Just d) _) ->
        "While\n" ++
          (show' (n+1) w) ++ "\n" ++
        (tabs n) ++ "Do\n" ++
          (show' (n+1) d)

      (RWD (Just r) w Nothing _) ->
        "Repeat\n" ++
          (show' (n+1) r) ++ "\n" ++
        (tabs n) ++ "While\n" ++
          (show' (n+1) w)

      (RWD (Just r) w (Just d) _) ->
        "Repeat\n" ++
          (show' (n+1) r) ++ "\n" ++
        (tabs n) ++ "While\n" ++
          (show' (n+1) w) ++ "\n" ++
        (tabs n) ++ "Do\n" ++
          (show' (n+1) d)

      (For (Id v _) d s i _) ->
        "For\n" ++
          (tabs (n+1)) ++ "variable\n" ++
            (tabs (n+2)) ++ v ++ "\n" ++
          (tabs (n+1)) ++ "direction\n" ++
            (tabs (n+2)) ++ show d ++ "\n" ++
          (tabs (n+1)) ++ "in\n" ++
            (show' (n+2) s) ++ "\n" ++
          (tabs (n+1)) ++ "do\n" ++
            (show' (n+2) i)

instance Show' Declare where
  show' n (Declare t v) =
    tabs n ++ show t ++ " " ++ show v

instance Show Type where
  show x =
    case x of
      BoolType    -> "bool"
      IntType     -> "int"
      SetType     -> "set"
      StrType     -> "string"
      TypeError _ -> "_"

instance Show Id where
  show (Id v _) = v

instance Show' Exp where
  show' n x = (tabs n) ++
    case x of
      (Binary op a b) ->
        show op ++ "\n" ++
          (show' (n+1) a) ++ "\n" ++
          (show' (n+1) b)

      (Unary  op a) ->
        show op ++ "\n" ++
          (show' (n+1) a)

      (Set    es _) ->
        "set\n" ++
          (intercalate "\n" (map (show' (n+1)) es))

      (IntConst  a) ->
        "int\n" ++
          (tabs (n+1)) ++ show a

      (BoolConst a) ->
        "boolean\n" ++
          (tabs (n+1)) ++ show a

      (StrConst  a) ->
        "string\n" ++
          (tabs (n+1)) ++ show a

      (Var       a) ->
        "variable\n" ++
          (tabs (n+1)) ++ (show a)

instance Show BinOp where
  show x =
    case x of
      (Plus     _) -> "Plus (+)"
      (Minus    _) -> "Minus (-)"
      (Times    _) -> "Times (*)"
      (Div      _) -> "Div (/)"
      (Mod      _) -> "Mod (%)"
      (SetUnion _) -> "SetUnion (++)"
      (SetMinus _) -> "SetMinus (\\)"
      (SetInter _) -> "SetInter (><)"
      (MapPlus  _) -> "MapPlus (<+>)"
      (MapMinus _) -> "MapMinus (<->)"
      (MapTimes _) -> "MapTimes (<*>)"
      (MapDiv   _) -> "MapDiv (</>)"
      (MapMod   _) -> "MapMod (<%>)"
      (CompLT   _) -> "Less Than (<)"
      (CompLE   _) -> "Less or equal (<=)"
      (CompGT   _) -> "Greater than (>)"
      (CompGE   _) -> "Greater or equal (>=)"
      (CompEQ   _) -> "Equals (==)"
      (CompNE   _) -> "Not equal (/=)"
      (CompAt   _) -> "Is Member Of (@)"
      (And      _) -> "Conjunction (and)"
      (Or       _) -> "Disjunction (or)"

instance Show UnOp where
  show x =
    case x of
      (SetMax   _) -> "SetMax (>?)"
      (SetMin   _) -> "SetMin (<?)"
      (SetSize  _) -> "SetSize ($?)"
      (Not      _) -> "Negation (not)"
      (Negative _) -> "Negative (-)"
