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
         | SetMax    Exp    Exp
         | SetMin    Exp    Exp
         | SetSize   Exp    Exp
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
         | Parens    Exp
         | Number    Token
         | Var       Token
         | Set       [Exp]
         | BoolTrue
         | BoolFalse
         deriving (Eq, Show)

data Inst = Assign       Token  Exp
          | BlockUsing   [Declare]  [Inst]
          | Block        [Inst]
          | Scan         Token
          | Print        [Printable]
          | IfElse       Exp    Inst    Inst
          | If           Exp    Inst
          | RWD          Inst   Exp     Inst
          | WhileDo      Exp    Inst
          | Repeat       Inst   Exp
          | ForMin       Token  Exp     Inst
          | ForMax       Token  Exp     Inst
          deriving (Eq, Show)

data Printable = Strng Token
               | Exp    Exp
               deriving (Eq, Show)

data Using  = Using [Declare] deriving (Eq, Show)

data Declare = Declare Type [Token] deriving (Eq, Show)

data Type = BoolType | IntType | SetType deriving (Eq, Show)


