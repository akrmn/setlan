{
module Lexer
(
    lexer,
    printError,
    alexScanTokens
) where
import Tokens
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  -- whitespace --
  $white+                   ;

  -- comments --
  "#".*                     ;

  -- language --
  program                   { tok TokenProgram }
  using                     { tok TokenUsing }
  in                        { tok TokenIn }
  =                         { tok TokenAssign }

  -- brackets --
  "{"                       { tok TokenCurlyOpen }
  "}"                       { tok TokenCurlyClose }
  "("                       { tok TokenParenOpen }
  ")"                       { tok TokenParenClose }

  -- types --
  int                       { tok TokenIntT }
  bool                      { tok TokenBoolT }
  set                       { tok TokenSetT }

  -- boolean constants --
  true                      { tok TokenTrue }
  false                     { tok TokenFalse }

  -- separators --
  ","                       { tok TokenComma }
  ";"                       { tok TokenSemicolon }

  -- operators --
  -- -- int --
  "+"                       { tok TokenPlus }
  "-"                       { tok TokenMinus }
  "*"                       { tok TokenTimes }
  "/"                       { tok TokenDiv }
  "%"                       { tok TokenMod }

  -- -- set  --
  "++"                      { tok TokenSetUnion }
  \\                        { tok TokenSetMinus }
  "><"                      { tok TokenSetInter }
  ">?"                      { tok TokenSetMax }
  "<?"                      { tok TokenSetMin }
  "$?"                      { tok TokenSetSize }

  -- -- map --
  "<+>"                     { tok TokenMapPlus }
  "<->"                     { tok TokenMapMinus }
  "<*>"                     { tok TokenMapTimes }
  "</>"                     { tok TokenMapDiv }
  "<%>"                     { tok TokenMapMod }

  -- -- bool --
  and                       { tok TokenAnd }
  or                        { tok TokenOr }
  not                       { tok TokenNot }

  -- -- relational --
  "<"                       { tok TokenLT }
  "<="                      { tok TokenLE }
  ">"                       { tok TokenGT }
  ">="                      { tok TokenGE }
  "=="                      { tok TokenEQ }
  "/="                      { tok TokenNE }
  @                         { tok TokenAt }

  -- control statements --
  if                        { tok TokenIf }
  else                      { tok TokenElse }
  for                       { tok TokenFor }
  min                       { tok TokenMin }
  max                       { tok TokenMax }
  repeat                    { tok TokenRepeat }
  while                     { tok TokenWhile }
  do                        { tok TokenDo }

  -- IO functions --
  scan                      { tok TokenScan }
  print                     { tok TokenPrint }
  println                   { tok TokenPrintln }

  -- variables --
  (\-?)$digit+              { lexInt }
  \"([^\"]|(\\\"))*\"       { toq TokenString read }

  $alpha[$alpha$digit\_\']* { toq TokenIdent id }

  -- error --
  .                         { toq TokenError id }

{

toPos :: AlexPosn -> Pos
toPos (AlexPn _ line column) = Pos line column

tok :: (Pos -> Token) -> AlexPosn -> String -> Token
tok f p _   = f (toPos p)

toq :: (a -> Pos -> Token) -> (String -> a) -> AlexPosn -> String -> Token
toq f g p s = f (g s) (toPos p)

lexInt :: AlexPosn -> String -> Token
lexInt p s
  | n < -2^31  = TokenIntError s (toPos p)
  | n < 2^31   = TokenInt      n (toPos p)
  | otherwise  = TokenIntError s (toPos p)
  where n = (read s :: (Num a, Read a) => a)

printError :: Token -> IO ()
printError (TokenError s p) = do
  putStrLn $ "Error: unexpected token \"" ++ s ++ "\" " ++ show p
printError (TokenIntError s p) = do
  putStrLn $ "Error: integer out of range (-2^31 .. 2^31-1) \"" ++ s ++ "\" " ++ show p

lexer :: String -> String -> IO ()
-- Calls the Alex token scanner and then prints found tokens. If there are any
-- TokenErrors, it only prints those.
lexer text name = do
  putStrLn $ "Lexer (" ++ name ++ "):\n"
  let toks = alexScanTokens text
  if any isTokenError toks
    then mapM_ printError $ filter isTokenError toks
    else mapM_ print toks
  putStrLn ""

}
