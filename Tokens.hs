-- CI3725 Traductores e interpretadores
-- Project part 1 - Lexer
-- Members:
--         Moises Ackerman 11-10005
--         Carlos Ferreira 11-10323

module Tokens where

data Pos = Pos Int Int deriving (Eq, Show)

data Token
    -- language --
    = TokenProgram Pos | TokenUsing Pos | TokenIn    Pos
    | TokenAssign  Pos | TokenDef   Pos | TokenArrow Pos
    | TokenReturn  Pos

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
    | TokenIf     Pos | TokenThen  Pos | TokenElse Pos
    | TokenFor    Pos | TokenMin   Pos | TokenMax  Pos
    | TokenRepeat Pos | TokenWhile Pos | TokenDo   Pos

    -- IO functions --
    | TokenScan Pos | TokenPrint Pos | TokenPrintln Pos

    -- variables --
    | TokenInt    Int    Pos
    | TokenString String Pos
    | TokenIdent  String Pos

    -- error --
    | TokenError  String Pos
    deriving (Eq, Show)

token_posn :: Token -> Pos
token_posn t = case t of
    -- language --
    (TokenProgram p) -> p
    (TokenUsing p) -> p
    (TokenIn p) -> p
    (TokenAssign p) -> p
    (TokenDef p) -> p
    (TokenArrow p) -> p
    (TokenReturn p) -> p

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
    (TokenThen p) -> p
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

    -- error --
    (TokenError _ p) -> p
