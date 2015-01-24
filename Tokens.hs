module Tokens where

data Pos = Pos Int Int deriving (Eq, Show)

data Token = TokenProgram             Pos
           | TokenUsing               Pos
           | TokenIn                  Pos
           | TokenIntT                Pos
           | TokenInt         Int     Pos
           | TokenBoolT               Pos
           | TokenTrue                Pos
           | TokenFalse               Pos
           | TokenSetT                Pos
           | TokenCurlyOpen           Pos
           | TokenCurlyClose          Pos
           | TokenParenOpen           Pos
           | TokenParenClose          Pos
           | TokenComma               Pos
           | TokenSemicolon           Pos
           | TokenColon               Pos
           | TokenAssign              Pos
           | TokenIntOp       String  Pos
           | TokenSetBinOp    String  Pos
           | TokenSetUnOp     String  Pos
           | TokenMapOp       String  Pos
           | TokenBoolOp      String  Pos
           | TokenRelOp       String  Pos
           | TokenIf                  Pos
           | TokenThen                Pos
           | TokenElse                Pos
           | TokenFor                 Pos
           | TokenMin                 Pos
           | TokenMax                 Pos
           | TokenRepeat              Pos
           | TokenWhile               Pos
           | TokenDo                  Pos
           | TokenDef                 Pos
           | TokenArrow               Pos
           | TokenReturn              Pos
           | TokenScan                Pos
           | TokenPrint               Pos
           | TokenPrintln             Pos
           | TokenString      String  Pos
           | TokenVar         String  Pos
           | TokenError       String  Pos
           deriving (Eq, Show)

isError :: Token -> Bool
isError t
    = case t of
        TokenError _ _ -> True
        _              -> False

noError :: [Token] -> Bool
noError l = null $ listErrors l

listErrors :: [Token] -> [Token]
listErrors l = filter isError l

token_posn :: Token -> Pos
token_posn t =
    case t of
        (TokenProgram p)        -> p
        (TokenUsing p)          -> p
        (TokenIn p)             -> p
        (TokenIntT p)           -> p
        (TokenInt _ p)          -> p
        (TokenBoolT p)          -> p
        (TokenTrue p)           -> p
        (TokenFalse p)          -> p
        (TokenSetT p)           -> p
        (TokenCurlyOpen p)      -> p
        (TokenCurlyClose p)     -> p
        (TokenParenOpen p)      -> p
        (TokenParenClose p)     -> p
        (TokenComma p)          -> p
        (TokenSemicolon p)      -> p
        (TokenColon p)          -> p
        (TokenAssign p)         -> p
        (TokenIntOp _ p)        -> p
        (TokenSetBinOp _ p)     -> p
        (TokenSetUnOp _ p)      -> p
        (TokenMapOp _ p)        -> p
        (TokenBoolOp _ p)       -> p
        (TokenRelOp _ p)        -> p
        (TokenIf p)             -> p
        (TokenThen p)           -> p
        (TokenElse p)           -> p
        (TokenFor p)            -> p
        (TokenMin p)            -> p
        (TokenMax p)            -> p
        (TokenRepeat p)         -> p
        (TokenWhile p)          -> p
        (TokenDo p)             -> p
        (TokenDef p)            -> p
        (TokenArrow p)          -> p
        (TokenReturn p)         -> p
        (TokenScan p)           -> p
        (TokenPrint p)          -> p
        (TokenPrintln p)        -> p
        (TokenString _ p)       -> p
        (TokenVar _ p)          -> p
        (TokenError _ p)        -> p
