{
module Parser (
    parser
) where
import Tokens
import AST
import Lexer

import Text.Show.Pretty
}

%name parsr
%tokentype { Token }
%error { parseError }

%token
    -- language --
    program     { TokenProgram _ }
    using       { TokenUsing _ }
    in          { TokenIn _ }
    '='         { TokenAssign _ }
    -- def         { TokenDef _ }
    -- '->'        { TokenArrow _ }
    -- return      { TokenReturn _ }

    -- brackets --
    '{'         { TokenCurlyOpen _ }
    '}'         { TokenCurlyClose _ }
    '('         { TokenParenOpen _ }
    ')'         { TokenParenClose _ }

    -- types --
    int         { TokenIntT _ }
    bool        { TokenBoolT _ }
    set         { TokenSetT _ }

    -- boolean constants --
    true        { TokenTrue _ }
    false       { TokenFalse _ }

    -- separators --
    ','         { TokenComma _ }
    ';'         { TokenSemicolon _ }

    -- operators --
    -- -- int --
    '+'         { TokenPlus _ }
    '-'         { TokenMinus _ }
    '*'         { TokenTimes _ }
    '/'         { TokenDiv _ }
    '%'         { TokenMod _ }

    -- -- set  --
    '++'        { TokenSetUnion _ }
    '\\'        { TokenSetMinus _ }
    '><'        { TokenSetInter _ }
    '>?'        { TokenSetMax _ }
    '<?'        { TokenSetMin _ }
    '$?'        { TokenSetSize _ }

    -- -- map --
    '<+>'       { TokenMapPlus _ }
    '<->'       { TokenMapMinus _ }
    '<*>'       { TokenMapTimes _ }
    '</>'       { TokenMapDiv _ }
    '<%>'       { TokenMapMod _ }

    -- -- bool --
    and         { TokenAnd _ }
    or          { TokenOr _ }
    not         { TokenNot _ }

    -- -- relational --
    '<'         { TokenLT _ }
    '<='        { TokenLE _ }
    '>'         { TokenGT _ }
    '>='        { TokenGE _ }
    '=='        { TokenEQ _ }
    '/='        { TokenNE _ }
    '@'         { TokenAt _ }

    -- control statements --
    if          { TokenIf _ }
    then        { TokenThen _ }
    else        { TokenElse _ }
    for         { TokenFor _ }
    min         { TokenMin _ }
    max         { TokenMax _ }
    repeat      { TokenRepeat _ }
    while       { TokenWhile _ }
    do          { TokenDo _ }

    -- IO functions --
    scan        { TokenScan _ }
    print       { TokenPrint _ }
    println     { TokenPrintln _ }

    -- variables --
    num         { TokenInt _ _ }
    str         { TokenString _ _ }
    id          { TokenIdent _ _ }

-- Operator precedence --
-- -- bool --
%left  or
%left  and

-- -- relational --
%nonassoc '<' '<=' '>' '>='
%nonassoc '==' '/='
%nonassoc '@'

-- -- int --
%left '+' '-'
%left '*' '/' '%'

-- -- set
%left '++' '\\'
%left '><'

-- -- map
%left '<+>' '<->'
%left '<*>' '</>' '<%>'

-- -- unary
%right '>?' '<?' '$?'
%right not
%right NEG

%%

Program : program Inst                  { Program $2 }

Exp : Exp '+'   Exp                     { Plus $1 $3 }
    | Exp '-'   Exp                     { Minus $1 $3 }
    | Exp '*'   Exp                     { Times $1 $3 }
    | Exp '/'   Exp                     { Div $1 $3 }
    | Exp '%'   Exp                     { Mod $1 $3 }
    | Exp '++'  Exp                     { SetUnion $1 $3 }
    | Exp '\\'  Exp                     { SetMinus $1 $3 }
    | Exp '><'  Exp                     { SetInter $1 $3 }
    | Exp '>?'  Exp                     { SetMax  $1 $3 }
    | Exp '<?'  Exp                     { SetMin  $1 $3 }
    | Exp '$?'  Exp                     { SetSize $1 $3 }
    | Exp '<+>' Exp                     { MapPlus $1 $3 }
    | Exp '<->' Exp                     { MapMinus $1 $3 }
    | Exp '<*>' Exp                     { MapTimes $1 $3 }
    | Exp '</>' Exp                     { MapDiv $1 $3 }
    | Exp '<%>' Exp                     { MapMod $1 $3 }
    | Exp '<'   Exp                     { CompLT $1 $3 }
    | Exp '<='  Exp                     { CompLE $1 $3 }
    | Exp '>'   Exp                     { CompGT $1 $3 }
    | Exp '>='  Exp                     { CompGE $1 $3 }
    | Exp '=='  Exp                     { CompEQ $1 $3 }
    | Exp '/='  Exp                     { CompNE $1 $3 }
    | Exp '@'   Exp                     { CompAt $1 $3 }
    | Exp and   Exp                     { And $1 $3 }
    | Exp or    Exp                     { Or $1 $3 }
    | not Exp                           { Not $2 }
    | '(' Exp ')'                       { Parens $2 }
    | num                               { Number $1 }
    | true                              { BoolTrue }
    | false                             { BoolFalse }
    | id                                { Var $1 }
    | '{' Conts '}'                     { Set $2 }

Conts : Conts ',' Exp                   { $1 ++ [$3] }
      | Exp                             { [$1] }
      |                                 { [] }

Insts : Insts Inst ';'                  { $1 ++ [$2] }
      |                                 { [] }

Inst : id '=' Exp                       { Assign  $1 $3 }
     | '{' using Declares in Insts '}'  { BlockUsing $3 $5 }
     | '{' Insts '}'                    { Block $2 }
     | scan id                          { Scan    $2 }
     | print Printables                 { Print   $2 }
     | println Printables               { Print  ($2 ++ [Strng newline]) }
     | if Exp Inst else Inst            { IfElse  $2 $3 $5 }
     | if Exp Inst                      { If      $2 $3 }
     | repeat Inst while Exp do Inst    { RWD     $2 $4 $6 }
     | while Exp do Inst                { WhileDo $2 $4 }
     | repeat Inst while Exp            { Repeat  $2 $4 }
     | for id min Exp do Inst           { ForMin $2 $4 $6 }
     | for id max Exp do Inst           { ForMax $2 $4 $6 }

Printables : Printables ',' Printable   { $1 ++ [$3] }
           | Printable                  { [$1] }

Printable : Exp                         { Exp    $1 }
          | str                         { Strng  $1 }

Declares : Declares Declare ';'         { $1 ++ [$2] }
         | Declare ';'                  { [$1] }

Declare : Type Variables                { Declare $1 $2 }

Type : bool                             { BoolType }
     | int                              { IntType }
     | set                              { SetType }

Variables : Variables ',' id            { $1 ++ [$3] }
          | id                          { [$1] }

{

newline = TokenString "\n" (Pos 0 0)

parseError :: [Token] -> a
parseError l = case l of
    [] -> error $ "Unexpected EOF."
    _  -> error $ "Parse error on " ++ show (head l) ++ "."

printAST :: [Token] -> IO()
printAST = putStrLn . ppShow . parsr

parser :: String -> IO ()
parser text = do
    let toks = alexScanTokens text
    if any isTokenError toks
        then mapM_ printError $ filter isTokenError toks
        else printAST toks

}
