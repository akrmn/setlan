module AST where

import Data.List (intersperse)

data Program = Program Inst deriving (Eq, Show)

data Inst
  = Assign String    Exp
  | Block  [Declare] [Inst]
  | Scan   String
  | Print  [Exp]
  | If Exp Inst (Maybe Inst)
  | RWD (Maybe Inst) Exp (Maybe Inst)
  | For String Direction Exp Inst
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
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  | SetUnion
  | SetMinus
  | SetInter
  | MapPlus
  | MapMinus
  | MapTimes
  | MapDiv
  | MapMod
  | CompLT
  | CompLE
  | CompGT
  | CompGE
  | CompEQ
  | CompNE
  | CompAt
  | And
  | Or
  deriving (Eq)

data UnOp
  = SetMax
  | SetMin
  | SetSize
  | Not
  | Negative
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
      (Assign v e) ->
        "Assign\n" ++
          (tabs (n+1)) ++ "variable\n" ++
            (tabs (n+2)) ++ v ++ "\n" ++
          (tabs (n+1)) ++ "value\n" ++
            (show'' (n+2) e)

      (Block [] is) ->
        "Block\n" ++
          (concat $ intersperse "\n" (map (show'' (n+1)) is))

      (Block ds is) ->
        "Block\n" ++
          (tabs (n+1)) ++ "Using\n" ++
            (concat $ intersperse "\n" (map (show'' (n+2)) ds)) ++ "\n" ++
          (tabs (n+1)) ++ "In\n" ++
          (concat $ intersperse "\n" (map (show'' (n+1)) is))

      (Scan v) ->
        "Scan\n" ++
          (tabs (n+1)) ++ "variable\n" ++
            (tabs (n+2)) ++ v

      (Print es) ->
        "Print\n" ++
          (tabs (n+1)) ++ "elements\n" ++
            (concat $ intersperse "\n" (map (show'' (n+2)) es))

      (If c t Nothing) ->
        "If\n" ++
          (tabs (n+1)) ++ "condition\n" ++
            (show'' (n+2) c) ++ "\n" ++
          (tabs (n+1)) ++ "Then\n" ++
            (show'' (n+2) t)

      (If c t (Just e)) ->
        "If\n" ++
          (tabs (n+1)) ++ "condition\n" ++
            (show'' (n+2) c) ++ "\n" ++
          (tabs (n+1)) ++ "Then\n" ++
            (show'' (n+2) t) ++ "\n" ++
          (tabs (n+1)) ++"Else\n" ++
            (show'' (n+2) e)

      (RWD Nothing w (Just d)) ->
        "While\n" ++
          (show'' (n+1) w) ++ "\n" ++
        (tabs n) ++ "Do\n" ++
          (show'' (n+1) d)

      (RWD (Just r) w Nothing) ->
        "Repeat\n" ++
          (show'' (n+1) r) ++ "\n" ++
        (tabs n) ++ "While\n" ++
          (show'' (n+1) w)

      (RWD (Just r) w (Just d)) ->
        "Repeat\n" ++
          (show'' (n+1) r) ++ "\n" ++
        (tabs n) ++ "While\n" ++
          (show'' (n+1) w) ++ "\n" ++
        (tabs n) ++ "Do\n" ++
          (show'' (n+1) d)

      (For v d s i) ->
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
      Plus     -> "Plus +"
      Minus    -> "Minus -"
      Times    -> "Times *"
      Div      -> "Div /"
      Mod      -> "Mod %"
      SetUnion -> "SetUnion ++"
      SetMinus -> "SetMinus \\"
      SetInter -> "SetInter ><"
      MapPlus  -> "MapPlus <+>"
      MapMinus -> "MapMinus <->"
      MapTimes -> "MapTimes <*>"
      MapDiv   -> "MapDiv </>"
      MapMod   -> "MapMod <%>"
      CompLT   -> "Less Than <"
      CompLE   -> "Less or equal <="
      CompGT   -> "Greater than >"
      CompGE   -> "Greater or equal >="
      CompEQ   -> "Equals =="
      CompNE   -> "Not equal /="
      CompAt   -> "Is Member Of @"
      And      -> "Conjunction And"
      Or       -> "Disjunction Or"

instance Show UnOp where
  show x =
    case x of
      SetMax   -> "SetMax >?"
      SetMin   -> "SetMin <?"
      SetSize  -> "SetSize $?"
      Not      -> "Negation Not"
      Negative -> "Negative (-)"
