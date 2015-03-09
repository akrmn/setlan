module Interpreter
( interpreter
) where

import qualified Data.Set as Set (
    Set(..)
  , difference
  , findMax
  , findMin
  , fold
  , fromList
  , intersection
  , map
  , member
  , null
  , size
  , toAscList
  , toDescList
  , union
  )
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import System.IO (hFlush, stdout)

import Tokens (Pos(..), isTokenError, printError)
import Lexer (alexScanTokens)
import AST(
    Direction(..)
  , Exp(..)
  , Inst(..)
  , Id(..)
  , BinOp(..)
  , UnOp(..)
  , Type(..)
  )
import Parser (parsr)
import SymbolTable (
    Variable(..)
  , SymbolTable(..)
  , update
  , deepLookup
  , deepUpdate
  , varType
  )
import Scoper (scoper'')

type Executor a = StateT [SymbolTable] IO a

interpreter :: String -> String -> IO ()
interpreter text name = do
  putStrLn $ "Interpreter (" ++ name ++ "):\n"
  let toks = alexScanTokens text
  if any isTokenError toks
    then mapM_ printError $ filter isTokenError toks
    else let scope = scoper'' . parsr $ toks
      in case scope of
        Left error -> putStr error
        Right st   -> interpret st
  putStrLn ""

interpret :: SymbolTable -> IO ()
interpret st@(SymbolTable _ _ d [i]) =
  case i of
    (Block _ _ _) -> evalStateT (execute i) [head d, st]
    _ -> evalStateT (execute i) [st]

maybeRead :: Read a => String -> Maybe a
maybeRead s =
  case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

execute :: Inst -> Executor ()

-- Assign Instruction --
execute (Assign (Id name _) value pos) =
  do
    sts <- get
    let sts' = deepUpdate name (evaluate sts value) sts
    put sts'


-- Block Instruction --
execute (Block declares insts pos) =
  do
    sts <- get
    execAll sts insts insts
  where
    execAll sts insts [] = return ()
    execAll sts insts (i:is) = do
      sts <- get
      execute' sts insts i
      execAll sts insts is

    execute' :: [SymbolTable] -> [Inst] -> Inst -> Executor ()
    execute' sts insts b@(Block _ _ _) = do
      put $ ((daughters $ head sts) !! (fromJust $ elemIndex b insts')):sts
      execute b
      sts <- get
      put (tail sts)
    execute' sts insts f@(For _ _ _ _ _) = do
      put $ ((daughters $ head sts) !! (fromJust $ elemIndex f insts')):sts
      execute f
      sts <- get
      put (tail sts)
    execute' sts _ i = do
      put sts
      execute i

    insts' = filter hasScope insts

    hasScope :: Inst -> Bool
    hasScope (Block _ _ _) = True
    hasScope (For _ _ _ _ _) = True
    hasScope (If _ _ _ _) = True
    hasScope (RWD _ _ _ _) = True
    hasScope _ = False

-- Scan Instruction --
execute (Scan (Id name _) pos) =
  do
    sts <- get
    let scanType = varType . fst . fromJust $ (deepLookup name sts)
    case scanType of
      IntType ->
        do
          input <- lift getLine
          case maybeRead input of
            Just x ->
              if checkInt x
                then
                  let sts' = deepUpdate name (IntVar x) sts
                  in put sts'
                else
                  error $ "Integer value out of bounds; `scan` at " ++
                  show pos ++ "."
            Nothing ->
              error $ "Couldn't parse integer value; `scan` at " ++
                show pos ++ "."
      BoolType ->
        do
          input <- lift getLine
          case maybeRead input of
            Just x ->
              let sts' = deepUpdate name (BoolVar x) sts
              in put sts'
            Nothing ->
              error $ "Couldn't parse Boolean value, `scan` at " ++
                show pos ++ "."


-- Print Instruction --
execute (Print exps pos) =
  do
    sts <- get
    let evaluated = concat $ map show $ map (evaluate sts) exps
    lift $ putStr evaluated
    lift $ hFlush stdout


-- If Instruction --
execute (If cond thn els pos) =
  do
    sts <- get
    let BoolVar c = evaluate sts cond
    if c
      then do execute thn
      else case els of
        Nothing -> return ()
        Just inst -> execute inst


-- RWD Instruction --
execute rwd@(RWD (Just r) cond (Just d) pos) =
  do
    execute r
    sts <- get
    let BoolVar c = evaluate sts cond
    if c
      then do
        execute d
        sts <- get
        let BoolVar c' = evaluate sts cond
        if c'
          then  execute rwd
          else return ()
      else return ()
execute rw@(RWD (Just r) cond Nothing pos) =
  do
    execute r
    sts <- get
    let BoolVar c = evaluate sts cond
    if c
      then execute rw
      else return ()
execute wd@(RWD Nothing cond (Just d) pos) =
  do
    sts <- get
    let BoolVar c = evaluate sts cond
    if c
      then do
        execute d
        execute wd
      else return ()


-- For Instruction --
execute (For (Id name _) direction range inst pos) =
  do
    sts <- get
    let elems = if direction == Min
        then Set.toAscList (getSet (evaluate sts range))
        else Set.toDescList (getSet (evaluate sts range))
    mapM_ (execute' inst) elems

  where
    execute' :: Inst -> Int -> Executor ()
    execute' i n = do
      st:sts <- get
      let sts' = ((update name (IntVar n) st):sts)
      put sts'
      execute i


evaluate :: [SymbolTable] -> Exp -> Variable

evaluate sts (Binary binOp exp0 exp1) =
  case binOp of
    Plus  p -> if okay
                then IntVar result
                else error $ "Overflow at " ++ show p ++ "."
              where
                okay = checkInt result
                result = int0 + int1
    Minus p -> if okay
                then IntVar result
                else error $ "Overflow at " ++ show p ++ "."
              where
                okay = checkInt result
                result = int0 - int1
    Times p -> if okay
                then IntVar result
                else error $ "Overflow at " ++ show p ++ "."
              where
                okay = checkInt result
                result = int0 * int1
    Div   p -> if int1 == 0
                  then error $ "Division by zero at " ++ show p ++ "."
                  else IntVar (int0 `div` int1)
    Mod   p -> if int1 == 0
               then error $ "Modulation by zero at " ++ show p ++ "."
               else IntVar (int0 `mod` int1)

    SetUnion p -> SetVar (Set.union        set0 set1)
    SetMinus p -> SetVar (Set.difference   set0 set1)
    SetInter p -> SetVar (Set.intersection set0 set1)

    MapPlus  p -> if okay
                    then SetVar result
                    else error $ "Overflow at " ++ show p ++ "."
                  where
                    okay = checkSet result
                    result = (
                      case type0 of
                        IntType -> Set.map (int0 +) set1
                        SetType -> Set.map (+ int1) set0
                      )
    MapMinus p -> if okay
                    then SetVar result
                    else error $ "Overflow at " ++ show p ++ "."
                  where
                    okay = checkSet result
                    result = (
                      case type0 of
                        IntType -> Set.map (int0 -) set1
                        SetType -> Set.map (subtract int1) set0
                      )
    MapTimes p -> if okay
                    then SetVar result
                    else error $ "Overflow at " ++ show p ++ "."
                  where
                    okay = checkSet result
                    result = (
                      case type0 of
                        IntType -> Set.map (int0 *) set1
                        SetType -> Set.map (* int1) set0
                      )
    MapDiv   p -> SetVar (case type0 of
                    IntType -> if 0 `Set.member` set1
                      then error "Division by zero."
                      else Set.map (int0 `div`) set1
                    SetType -> if int1 == 0
                      then error "Division by zero."
                      else Set.map (`div` int1) set0
                  )
    MapMod   p -> SetVar (case type0 of
                    IntType -> if 0 `Set.member` set1
                      then error "Modulation by zero."
                      else Set.map (int0 `mod`) set1
                    SetType -> if int1 == 0
                      then error "Modulation by zero."
                      else Set.map (`mod` int1) set0
                  )

    CompLT p -> BoolVar (int0   <    int1)
    CompLE p -> BoolVar (int0   <=   int1)
    CompGT p -> BoolVar (int0   >    int1)
    CompGE p -> BoolVar (int0   >=   int1)

    CompEQ p -> BoolVar (val0   ==   val1)
    CompNE p -> BoolVar (val0   /=   val1)

    CompAt p -> BoolVar (int0 `Set.member` set1)

    And p -> BoolVar (bool0 && bool1)
    Or  p -> BoolVar (bool0 || bool1)

  where
    val0  = evaluate sts exp0
    int0  = getInt  val0
    bool0 = getBool val0
    set0  = getSet  val0
    type0 = varType val0

    val1  = evaluate sts exp1
    int1  = getInt  val1
    bool1 = getBool val1
    set1  = getSet  val1
    type1 = varType val1

evaluate sts (Unary unOp exp0) =
  case unOp of
    SetMax   p -> if Set.null set0
                    then error $ "Max of empty set at " ++ show p ++ "."
                    else IntVar (Set.findMax set0)
    SetMin   p -> if Set.null set0
                    then error $ "Min of empty set at " ++ show p ++ "."
                    else IntVar (Set.findMin set0)
    SetSize  p -> IntVar (Set.size set0)

    Not      p -> BoolVar (not bool0)

    Negative p -> IntVar (negate int0)

  where
    val0  = evaluate sts exp0
    int0  = getInt  val0
    bool0 = getBool val0
    set0  = getSet  val0
    type0 = varType val0

evaluate sts (Set exps _) =
  SetVar list
  where
    list = Set.fromList ints
    ints = map getInt vars
    vars = map (evaluate sts) exps

evaluate _ (BoolConst b) = BoolVar b

evaluate _ (IntConst i)  = IntVar i

evaluate _ (StrConst s)  = StrVar s

evaluate sts (Var (Id var pos)) =
  case deepLookup var sts of
    Just x -> fst x
    Nothing -> error (show sts)

checkInt :: Int -> Bool
checkInt n
  | n < -2^31  = False
  | n < 2^31   = True
  | otherwise  = False

checkSet :: (Set.Set Int) -> Bool
checkSet = Set.fold (\x acc -> (checkInt x) && acc) True
