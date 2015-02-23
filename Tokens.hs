module Tokens where

data Pos = Pos Int Int deriving Eq
instance Show Pos where
    show (Pos l c) = "(Line " ++ show l ++ ", Col " ++ show c ++ ")"

error' :: Pos -> String -> a
error' p m = error $ m ++ " (" ++ show pos ++ ")"

data Token
    -- language --
    = TokenProgram Pos | TokenUsing Pos   | TokenIn    Pos
    | TokenAssign  Pos

    -- brackets --
    | TokenCurlyOpen Pos | TokenCurlyClose Pos
    | TokenParenOpen Pos | TokenParenClose Pos

    -- types --
    | TokenIntT Pos | TokenBoolT Pos | TokenSetT Pos

    -- boolean constants --
    | TokenTrue Pos | TokenFalse Pos

    -- separators --
    | TokenComma Pos | TokenSemicolon Pos

    -- operators --
    -- -- int --
    | TokenPlus Pos | TokenMinus Pos | TokenTimes Pos
    | TokenDiv  Pos | TokenMod   Pos

    -- -- set  --
    | TokenSetUnion Pos | TokenSetMinus Pos | TokenSetInter Pos
    | TokenSetMax   Pos | TokenSetMin   Pos | TokenSetSize  Pos

    -- -- map --
    | TokenMapPlus  Pos | TokenMapMinus Pos | TokenMapTimes Pos
    | TokenMapDiv   Pos | TokenMapMod   Pos

    -- -- bool --
    | TokenAnd Pos | TokenOr Pos | TokenNot Pos

    -- -- relational --
    | TokenLT Pos | TokenLE Pos | TokenGT Pos | TokenGE Pos
    | TokenEQ Pos | TokenNE Pos | TokenAt Pos

    -- control statements --
    | TokenIf    Pos | TokenElse Pos | TokenFor    Pos
    | TokenMin   Pos | TokenMax  Pos | TokenRepeat Pos
    | TokenWhile Pos | TokenDo   Pos

    -- IO functions --
    | TokenScan Pos | TokenPrint Pos | TokenPrintln Pos

    -- variables --
    | TokenInt    Int    Pos
    | TokenString String Pos
    | TokenIdent  String Pos

    -- errors --
    | TokenError    String Pos
    | TokenIntError String Pos
    deriving (Eq, Show)

token_posn :: Token -> Pos
token_posn t = case t of
    -- language --
    (TokenProgram p) -> p
    (TokenUsing p) -> p
    (TokenIn p) -> p
    (TokenAssign p) -> p

    -- brackets --
    (TokenCurlyOpen p) -> p
    (TokenCurlyClose p) -> p
    (TokenParenOpen p) -> p
    (TokenParenClose p) -> p

    -- types --
    (TokenIntT p) -> p
    (TokenBoolT p) -> p
    (TokenSetT p) -> p

    -- boolean constants --
    (TokenTrue p) -> p
    (TokenFalse p) -> p

    -- separators --
    (TokenComma p) -> p
    (TokenSemicolon p) -> p

    -- operators --
    -- -- int --
    (TokenPlus p) -> p
    (TokenMinus p) -> p
    (TokenTimes p) -> p
    (TokenDiv p) -> p
    (TokenMod p) -> p

    -- -- set  --
    (TokenSetUnion p) -> p
    (TokenSetMinus p) -> p
    (TokenSetInter p) -> p
    (TokenSetMax p) -> p
    (TokenSetMin p) -> p
    (TokenSetSize p) -> p

    -- -- map --
    (TokenMapPlus p) -> p
    (TokenMapMinus p) -> p
    (TokenMapTimes p) -> p
    (TokenMapDiv p) -> p
    (TokenMapMod p) -> p

    -- -- bool --
    (TokenAnd p) -> p
    (TokenOr p) -> p
    (TokenNot p) -> p

    ---- -- relational --
    (TokenLT p) -> p
    (TokenLE p) -> p
    (TokenGT p) -> p
    (TokenGE p) -> p
    (TokenEQ p) -> p
    (TokenNE p) -> p
    (TokenAt p) -> p

    -- control statements --
    (TokenIf p) -> p
    (TokenElse p) -> p
    (TokenFor p) -> p
    (TokenMin p) -> p
    (TokenMax p) -> p
    (TokenRepeat p) -> p
    (TokenWhile p) -> p
    (TokenDo p) -> p

    -- IO functions --
    (TokenScan p) -> p
    (TokenPrint p) -> p
    (TokenPrintln p) -> p

    -- variables --
    (TokenInt _ p) -> p
    (TokenString _ p) -> p
    (TokenIdent  _ p) -> p

    -- errors --
    (TokenError _ p) -> p
    (TokenIntError _ p) -> p

isTokenError :: Token -> Bool
isTokenError (TokenError    _ _) = True
isTokenError (TokenIntError _ _) = True
isTokenError _                   = False
