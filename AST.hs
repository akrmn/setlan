module AST where

import Tokens

data Program = Program Inst deriving ((Show))

data Exp = IntExp IntExp | SetExp SetExp | BoolExp BoolExp deriving ((Show))

data IntExp  = BinIntOp   String IntExp IntExp
             | Negative   IntExp
             | UnSetIntOp String SetExp
             | IntVar     Var
             | Number     String
             deriving ((Show))

data SetExp  = BinSetOp   String SetExp SetExp
             | BinMapOpSI String SetExp IntExp
             | BinMapOpIS String IntExp SetExp
             | SetVar     Var
             | Set        [IntExp]
             deriving ((Show))

data BoolExp = BinBoolOp String BoolExp BoolExp
             | UnBoolOp  String BoolExp
             | IntRel    String IntExp  IntExp
             | SetRel    String SetExp  SetExp
             | BoolRel   String BoolExp BoolExp
             | IntSetRel String IntExp  SetExp
             | BoolVar   Var
             | Boolean   Bool
             deriving ((Show))

data StrExp  = Strng String deriving ((Show))
data Var     = Var String deriving ((Show))

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
          deriving ((Show))

data Direction = Min | Max deriving ((Show))

data Using  = Using [Declare] deriving ((Show))

data Declare = Declare Type [Token] deriving ((Show))

data Type = BoolType | IntType | SetType deriving ((Show))
