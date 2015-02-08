module AST where

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
         | Number    String
         | Var       String
         | Set       [Exp]
         | Boolean   Bool
         | Strng     String
         deriving (Eq, Show)

tabs :: Int -> String
tabs = concat . flip replicate "  "

-- show' :: Exp -> String
-- show' Plus      Exp    Exp
-- show' Minus     Exp    Exp
-- show' Times     Exp    Exp
-- show' Div       Exp    Exp
-- show' Mod       Exp    Exp
-- show' SetUnion  Exp    Exp
-- show' SetMinus  Exp    Exp
-- show' SetInter  Exp    Exp
-- show' SetMax    Exp
-- show' SetMin    Exp
-- show' SetSize   Exp
-- show' MapPlus   Exp    Exp
-- show' MapMinus  Exp    Exp
-- show' MapTimes  Exp    Exp
-- show' MapDiv    Exp    Exp
-- show' MapMod    Exp    Exp
-- show' CompLT    Exp    Exp
-- show' CompLE    Exp    Exp
-- show' CompGT    Exp    Exp
-- show' CompGE    Exp    Exp
-- show' CompEQ    Exp    Exp
-- show' CompNE    Exp    Exp
-- show' CompAt    Exp    Exp
-- show' And       Exp    Exp
-- show' Or        Exp    Exp
-- show' Not       Exp
-- show' Negative  Exp
-- show' Number    Token
-- show' Var       Token
-- show' Set       [Exp]
-- show' BoolTrue
-- show' BoolFalse
-- show' Strng Token

data Inst = Assign  String  Exp
          | Block   {
                      using :: Maybe [Declare],
                      instructions :: [Inst]
                    }
          | Scan    String
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
                      variable :: String,
                      direction :: Direction,
                      set :: Exp,
                      instruction :: Inst
                    }
          deriving (Eq, Show)

data Direction = Min | Max deriving (Eq, Show)

data Using  = Using [Declare] deriving (Eq, Show)

data Declare = Declare Type [String] deriving (Eq, Show)

data Type = BoolType | IntType | SetType deriving (Eq, Show)
