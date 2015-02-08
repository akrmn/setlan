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

Exp : IntExp {$1} | SetExp {$1} | BoolExp {$1}

Exps : Exp ',' Exps                     { $1 : $3 }
     | Exp                              { [$1] }
     |                                  { [] }

StrAtom : str                           { Strng (extract $1) }

IntExp  : IntExp BinIntOp IntExp        { BinIntOp   $2 $1 $3 }
        | '-' IntExp %prec NEG          { Negative   $2 }
        | UnSetIntOp SetExp             { UnSetIntOp $1 $2 }
        | '(' IntExp ')'                { $2 }
        | IntAtom                       { $1 }

IntAtom  : Var                          { IntVar $1 }
         | num                          { Number (extract $1) }
BinIntOp : '+' { "+" } | '-' { "-" } | '*' { "*" } | '/' { "/" } | '%' { "%" }
UnSetIntOp : '>?' {">?"} | '<?' {"<?"} | '$?' {"$?"}

SetExp : SetExp BinSetOp SetExp         { BinSetOp   $2 $1 $3 }
       | SetExp BinMapOp IntExp         { BinMapOpSI $2 $1 $3 }
       | IntExp BinMapOp SetExp         { BinMapOpIS $2 $1 $3 }
       | '(' SetExp ')'                 { $2 }
       | SetAtom                        { $1 }

SetAtom  : Var                          { SetVar $1 }
         | '{' Conts '}'                { Set $2 }
Conts : IntExp ',' Conts                { $1 : $3 }
      | IntExp                          { [$1] }
      |                                 { [] }
BinSetOp : '++' {"++"} | '\\' {"\\"} | '><' {"><"}
BinMapOp : '<+>' {"<+>"} | '<->' {"<->"} | '<*>' {"<*>"} | '</>' {"</>"}
         | '<%>' {"<%>"}

BoolExp : BoolExp BinBoolOp BoolExp      { BinBoolOp $2 $1 $3 }
        | UnBoolOp BoolExp               { UnBoolOp  $1 $2 }
        | IntExp IntRel IntExp           { IntRel    $2 $1 $3 }
        | SetExp EqRel SetExp            { SetRel    $2 $1 $3 }
        | BoolExp EqRel BoolExp          { BoolRel   $2 $1 $3 }
        | IntExp IntSetRel SetExp        { IntSetRel $2 $1 $3 }
        | '(' BoolExp ')'                { $2 }
        | BoolAtom                       { $1 }

BoolAtom  : Var                          { BoolVar $1 }
          | Bool                         { Boolean $1 }
Bool      : true {True} | false {False}
BinBoolOp : and {"and"} | or  {"or"}
UnBoolOp  : not {"not"}
IntRel    : '<'  {"<"} | '<=' {"<="} | '>'  {">"} | '>=' {">="}
          | EqRel {$1}
EqRel     : '==' {"=="} | '/=' {"/="}
IntSetRel : '@' {"@"}

Var : id { Var (extract $1) }

Insts : Inst ';' Insts                   { $1 : $3 }
      |                                  { [] }

Inst : id '=' Exp                        { Assign  $1 $3 }

     | '{' using Declares in Insts '}'   { Block (Just $3) $5 }
     | '{' Insts '}'                     { Block Nothing   $2 }

     | scan id                           { Scan    $2 }
     | print Exps                        { Print   $2 }
     | println Exps                      { Print  ($2 ++ [Strng newline]) }

     | if BoolExp Inst else Inst         { If  $2 $3 (Just $5) }
     | if BoolExp Inst                   { If  $2 $3 Nothing }

     | repeat Inst while BoolExp do Inst { RWD     $2 $4 $6 }
     | while BoolExp do Inst             { WhileDo $2 $4 }
     | repeat Inst while BoolExp         { Repeat  $2 $4 }

     | for id Dir SetExp do Inst         { For $2 $3 $4 $6 }

Dir : min { Min }
    | max { Max }

Declares : Declare ';' Declares          { $1 : $3 }
         | Declare ';'                   { [$1] }

Declare : Type Variables                 { Declare $1 $2 }

Type : bool                              { BoolType }
     | int                               { IntType }
     | set                               { SetType }

Variables : id ',' Variables             { $1 : $3 }
          | id                           { [$1] }

{

extract :: Token -> String
extract (TokenInt    s _) = s
extract (TokenIdent  s _) = s
extract (TokenString s _) = s

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
