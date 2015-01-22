{
module Lexer (AlexPosn(..), alexScanTokens, token_posn) where
import Tokens
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
    $white+                             ;
    "#".*                               ;
    program                             { tok TokenProgram }
    using                               { tok TokenUsing }
    in                                  { tok TokenIn }
    int                                 { tok TokenIntT }
    $digit+                             { toq TokenInt read }
    bool                                { tok TokenBoolT }
    [Tt]rue                             { tok TokenTrue }
    [Ff]alse                            { tok TokenFalse }
    set                                 { tok TokenSetT }
    [\{]                                { tok TokenCurlyOpen }
    [\}]                                { tok TokenCurlyClose }
    [\(]                                { tok TokenParenOpen }
    [\)]                                { tok TokenParenClose }
    [\,]                                { tok TokenComma }
    [\;]                                { tok TokenSemicolon }
    [\:]                                { tok TokenColon }
    [=]                                 { tok TokenAssign }
    [\+\-\*\/\%]                        { toq TokenIntOp id }
    (\+\+)|(\\)|(\>\<)                  { toq TokenSetBinOp id }
    [\>\<\$]\?                          { toq TokenSetUnOp id }
    \<[\+\-\*\/\%]\>                    { toq TokenMapOp id }
    (and)|(or)|(not)                    { toq TokenBoolOp id }
    (\<=?)|(\>=?)|(==)|(\/=)|@          { toq TokenRelOp id }
    if                                  { tok TokenIf }
    then                                { tok TokenThen }
    else                                { tok TokenElse }
    for                                 { tok TokenFor }
    min                                 { tok TokenMin }
    max                                 { tok TokenMax }
    repeat                              { tok TokenRepeat }
    while                               { tok TokenWhile }
    do                                  { tok TokenDo }
    def                                 { tok TokenDef }
    \-\>                                { tok TokenArrow }
    return                              { tok TokenReturn }
    scan                                { tok TokenScan }
    print                               { tok TokenPrint }
    println                             { tok TokenPrintln }
    \"[^\"]*\"                          { toq TokenString read }
    $alpha [$alpha $digit \_ \']*       { toq TokenVar id }
    .                                   { toq TokenError id }

{

tok  f p s = f (toPos p)
toq f g p s = f (g s) (toPos p)

toPos :: AlexPosn -> Pos
toPos (AlexPn _ line column) = Pos line column

}
