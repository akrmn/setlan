module AST where

import Tokens

data Program = Program Inst deriving (Eq, Show)

data Exp = Plus      Exp    Exp
         | Minus     Exp    Exp
         | Times     Exp    Exp
         | Div       Exp    Exp
         | Mod       Exp    Exp
         | SetUnion  Exp    Exp
         | SetMinus  Exp    Exp
         | SetInter  Exp    Exp
         | SetMax    Exp
         | SetMin    Exp
         | SetSize   Exp
         | MapPlus   Exp    Exp
         | MapMinus  Exp    Exp
         | MapTimes  Exp    Exp
         | MapDiv    Exp    Exp
         | MapMod    Exp    Exp
         | CompLT    Exp    Exp
         | CompLE    Exp    Exp
         | CompGT    Exp    Exp
         | CompGE    Exp    Exp
         | CompEQ    Exp    Exp
         | CompNE    Exp    Exp
         | CompAt    Exp    Exp
         | And       Exp    Exp
         | Or        Exp    Exp
         | Not       Exp
         | Negative  Exp
         | Number    Token
         | Var       Token
         | Set       [Exp]
         | BoolTrue
         | BoolFalse
         | Strng Token
         deriving (Eq, Show)

data Inst = Assign  Token  Exp
          | Block   {
                      using :: Maybe [Declare],
                      instructions :: [Inst]
                    }
          | Scan    Token
          | Print   [Exp]
          | If      {
                      condition :: Exp,
                      then_ :: Inst,
                      else_ :: Maybe Inst
                    }
          | RWD     Inst   Exp     Inst
          | WhileDo Exp    Inst
          | Repeat  Inst   Exp
          | For     {
                      variable :: Token,
                      direction :: Direction,
                      set :: Exp,
                      instruction :: Inst
                    }
          deriving (Eq, Show)

data Direction = Min | Max deriving (Eq, Show)

data Using  = Using [Declare] deriving (Eq, Show)

data Declare = Declare Type [Token] deriving (Eq, Show)

data Type = BoolType | IntType | SetType deriving (Eq, Show)
