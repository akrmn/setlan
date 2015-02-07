-- CI3725 Traductores e interpretadores
-- Project part 1 - Lexer
-- Members:
--         Moises Ackerman 11-10005
--         Carlos Ferreira 11-10323

{
module Lexer (lexer) where
import Tokens
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
    -- whitespace --
    $white+     ;

    -- comments --
    "#".*       ;

    -- language --
    program     { tok TokenProgram }
    using       { tok TokenUsing }
    in          { tok TokenIn }
    =           { tok TokenAssign }
    def         { tok TokenDef }
    \-\>        { tok TokenArrow }          -- ->
    return      { tok TokenReturn }

    -- brackets --
    [\{]        { tok TokenCurlyOpen }      -- {
    [\}]        { tok TokenCurlyClose }     -- }
    [\(]        { tok TokenParenOpen }      -- (
    [\)]        { tok TokenParenClose }     -- )

    -- types --
    int         { tok TokenIntT }
    bool        { tok TokenBoolT }
    set         { tok TokenSetT }

    -- boolean constants --
    true        { tok TokenTrue }
    false       { tok TokenFalse }

    -- separators --
    [\,]        { tok TokenComma }          -- ,
    [\;]        { tok TokenSemicolon }      -- ;

    -- operators --
    -- -- int --
    \+          { tok TokenPlus }           -- +
    \-          { tok TokenMinus }          -- -
    \*          { tok TokenTimes }          -- *
    \/          { tok TokenDiv }            -- /
    \%          { tok TokenMod }            -- %

    -- -- set  --
    \+\+        { tok TokenSetUnion }       -- ++
    \\          { tok TokenSetMinus }       -- \
    \>\<        { tok TokenSetInter }       -- ><
    \>\?        { tok TokenSetMax }         -- >?
    \<\?        { tok TokenSetMin }         -- <?
    \$\?        { tok TokenSetSize }        -- $?

    -- -- map --
    \<\+\>      { tok TokenMapPlus }        -- <+>
    \<\-\>      { tok TokenMapMinus }       -- <->
    \<\*\>      { tok TokenMapTimes }       -- <*>
    \<\/\>      { tok TokenMapDiv }         -- </>
    \<\%\>      { tok TokenMapMod }         -- <%>

    -- -- bool --
    and         { tok TokenAnd }
    or          { tok TokenOr }
    not         { tok TokenNot }

    ---- -- relational --
    \<          { tok TokenLT }             -- <
    \<=         { tok TokenLE }             -- <=
    \>          { tok TokenGT }             -- >
    \>=         { tok TokenGE }             -- >=
    ==          { tok TokenEQ }             -- ==
    \/=         { tok TokenNE }             -- /=
    @           { tok TokenAt }

    -- control statements --
    if          { tok TokenIf }
    then        { tok TokenThen }
    else        { tok TokenElse }
    for         { tok TokenFor }
    min         { tok TokenMin }
    max         { tok TokenMax }
    repeat      { tok TokenRepeat }
    while       { tok TokenWhile }
    do          { tok TokenDo }

    -- IO functions --
    scan        { tok TokenScan }
    print       { tok TokenPrint }
    println     { tok TokenPrintln }

    -- variables --
    $digit+                   { toq TokenInt read }
    \"([^\"]|(\"))*\"         { toq TokenString read }
    $alpha[$alpha$digit\_\']* { toq TokenIdent id }

    -- error --
    .           { toq TokenError id }

{

tok  f p s = f (toPos p)
toq f g p s = f (g s) (toPos p)

toPos :: AlexPosn -> Pos
toPos (AlexPn _ line column) = Pos line column

hasError :: [Token] -> Bool
-- Checks whether a list of tokens contains a TokenError
hasError []                   = False
hasError (TokenError _ _ : l) = True
hasError (_ : l )             = hasError l

printError :: Token -> IO ()
printError (TokenError s p) = do
    putStrLn $ "Error: unexpected token \"" ++ s ++ "\" " ++ show p

lexer :: String -> IO ()
-- Calls the Alex token scanner and then prints found tokens. If there are any
-- TokenErrors, it only prints those.
lexer text = do
    let toks = alexScanTokens text
    if hasError toks
        then mapM_ printError $ filter isTokenError toks
        else mapM_ print toks

}
