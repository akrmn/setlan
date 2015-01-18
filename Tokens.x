{
module Main (Token(..), AlexPosn(..), alexScanTokens, token_posn, main) where
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                           ;
  "--".*                            ;
  let                               { tok (\p s -> TokLet p) }
  in                                { tok (\p s -> TokIn p) }
  $digit+                           { tok (\p s -> TokInt p (read s)) }
  [\=\+\-\*\/\(\)]                  { tok (\p s -> TokSym p (head s)) }
  $alpha [$alpha $digit \_ \']*     { tok (\p s -> TokVar p s) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f p s = f p s

-- The token type:
data Token = TokLet AlexPosn
           | TokIn  AlexPosn
           | TokSym AlexPosn Char
           | TokVar AlexPosn String
           | TokInt AlexPosn Int
           deriving (Eq,Show)

token_posn (Let p)   = p
token_posn (In p)    = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p

main = do
  s <- getContents
  print (alexScanTokens s)
}
