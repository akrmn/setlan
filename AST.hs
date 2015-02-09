module AST where

import Data.List (intersperse)

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

data Inst = Assign  {
                      variable :: String,
                      value :: Exp
                    }
          | Block   {
                      using :: Maybe Using,
                      instructions :: [Inst]
                    }
          | Scan    String
          | Print   [Exp]
          | If      {
                      condition :: Exp,
                      then_ :: Inst,
                      else_ :: Maybe Inst
                    }
          | RWD     {
                      repeat_ :: Inst,
                      while_ :: Exp,
                      do_ :: Inst
                    }
          | WhileDo {
                      while_ :: Exp,
                      do_ :: Inst
                    }
          | Repeat  {
                      repeat_ :: Inst,
                      while_ :: Exp
                    }
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

instance Show'' Exp where
  show'' n x = (tabs n) ++
    case x of
      (Plus      a    b) -> "Sum +\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (Minus     a    b) -> "Subtraction - \n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (Times     a    b) -> "Multiplication *\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (Div       a    b) -> "Division /\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (Mod       a    b) -> "Modulation %\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (SetUnion  a    b) -> "Union ++\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (SetMinus  a    b) -> "Difference \\\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (SetInter  a    b) -> "Intersection ><\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (SetMax    a)      -> "Max >?\n" ++ (show'' (n+1) a)
      (SetMin    a)      -> "Min <?\n" ++ (show'' (n+1) a)
      (SetSize   a)      -> "Size $?\n" ++ (show'' (n+1) a)
      (MapPlus   a    b) -> "MapSum <+>\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (MapMinus  a    b) -> "MapSubtraction <->\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (MapTimes  a    b) -> "MapMultiplication <*>\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (MapDiv    a    b) -> "MapDivision </>\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (MapMod    a    b) -> "MapModulation <%>\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (CompLT    a    b) -> "Less Than <\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (CompLE    a    b) -> "Less or Equal <=\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (CompGT    a    b) -> "Greater Than >\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (CompGE    a    b) -> "Greater or Equal >=\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (CompEQ    a    b) -> "Equals ==\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (CompNE    a    b) -> "Not Equal /=\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (CompAt    a    b) -> "Member of @\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (And       a    b) -> "Conjunction and\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (Or        a    b) -> "Disjunction or\n" ++ (show'' (n+1) a) ++ "\n" ++ (show'' (n+1) b)
      (Not       a)      -> "Negation not\n" ++ show'' (n+1) a
      (Negative  a)      -> "Negative (-)\n" ++ show'' (n+1) a
      (Number    a)      -> "Int\n" ++ (tabs (n+1)) ++ a
      (Var       a)      -> "Variable\n" ++ (tabs (n+1)) ++ a
      (Set       es)     -> "Set\n" ++ (concat $ intersperse "\n" (map (show'' (n+1)) es))
      (Boolean   a)      -> "Boolean\n" ++ (tabs (n+1)) ++ show a
      (Strng     a)      -> "String\n" ++ (tabs (n+1)) ++ show a

instance Show'' Inst where
  show'' n x = (tabs n) ++
    case x of
      (Assign v e)        -> "Assign\n" ++ (tabs (n+1)) ++ "variable\n" ++
                              (tabs (n+2)) ++ (show v) ++ "\n" ++ (tabs (n+1)) ++
                              "value\n" ++ (show'' (n+2) e)
      (Block Nothing is)  -> "Block\n" ++
                              (concat $ intersperse "\n" (map (show'' (n+1)) is))

      (Block (Just u) is) -> "Block\n" ++ (show'' (n+1) u) ++ "\n" ++
                              (concat $ intersperse "\n" (map (show'' (n+1)) is))


      (Scan v)            -> "Scan\n" ++ (tabs (n+1)) ++ "variable\n" ++
                              (tabs (n+2)) ++ show v
      (Print es)          -> "Print\n" ++ (tabs (n+1)) ++ "elements\n" ++
                              (concat $ intersperse "\n" (map (show'' (n+2)) es))
      (If c t Nothing)    -> "If\n" ++ (show'' (n+1) c) ++ "\n" ++
                              (tabs (n+1)) ++ "Then\n" ++ (show'' (n+2) t)
      (If c t (Just e))   -> "If\n" ++ (show'' (n+1) c) ++ "\n" ++
                              (tabs (n+1)) ++ "Then\n" ++ (show'' (n+2) t) ++
                              "Else\n" ++ (show'' (n+2) e)
      (RWD r w d)         -> "Repeat\n" ++ (show'' (n+1) r) ++ "\n" ++ (tabs n) ++
                              "While\n" ++ (show'' (n+1) w) ++ "\n" ++ (tabs n) ++
                              "Do\n" ++ (show'' (n+1) d)
      (WhileDo w d)       -> "While\n" ++ (show'' (n+1) w) ++ "\n" ++ (tabs n) ++
                              "Do\n" ++ (show'' (n+1) d)
      (Repeat r w)        -> "Repeat\n" ++ (show'' (n+1) r) ++ "\n" ++ (tabs n) ++
                              "While\n" ++ (show'' (n+1) w)
      (For v d s i)       -> "For\n" ++ (tabs (n+1)) ++ "variable\n" ++
                              (tabs (n+2)) ++ v ++ "\n" ++ (tabs (n+1)) ++
                              "direction" ++ (show'' (n+2) d) ++ "\n" ++
                              (tabs (n+1)) ++ "in" ++ (show'' (n+2) s) ++
                              "\n" ++ (tabs (n+1)) ++ "do\n" ++
                              (show'' (n+2) i)

instance Show'' Direction where
  show'' n x = (tabs n) ++ (show x)

instance Show'' Using where
  show'' n (Using ds) = (tabs n) ++ "Using\n" ++
    (concat $ intersperse "\n" (map (show'' (n+1)) ds)) ++ "\n" ++
    (tabs n) ++ "in"

instance Show'' Declare where
  show'' n (Declare t vs) = (show'' (n) t) ++ "\n" ++
    (concat $ intersperse "\n" (map (tabs (n+1) ++) vs))

instance Show'' Type where
  show'' n x = (tabs n) ++
    case x of
      (BoolType) -> "bool"
      (IntType)  -> "int"
      (SetType)  -> "set"
