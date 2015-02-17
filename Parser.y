{
module Parser (
  parser
) where
import Tokens
import AST
import Lexer
}

%name parsr
%tokentype {Token}
%error {parseError}

%token
  -- language --
  program     {TokenProgram _}
  using       {TokenUsing _}
  in          {TokenIn _}
  '='         {TokenAssign _}

  -- brackets --
  '{'         {TokenCurlyOpen _}
  '}'         {TokenCurlyClose _}
  '('         {TokenParenOpen _}
  ')'         {TokenParenClose _}

  -- types --
  int         {TokenIntT _}
  bool        {TokenBoolT _}
  set         {TokenSetT _}

  -- boolean constants --
  true        {TokenTrue _}
  false       {TokenFalse _}

  -- separators --
  ','         {TokenComma _}
  ';'         {TokenSemicolon _}

  -- operators --
  -- -- int --
  '+'         {TokenPlus _}
  '-'         {TokenMinus _}
  '*'         {TokenTimes _}
  '/'         {TokenDiv _}
  '%'         {TokenMod _}

  -- -- set  --
  '++'        {TokenSetUnion _}
  '\\'        {TokenSetMinus _}
  '><'        {TokenSetInter _}
  '>?'        {TokenSetMax _}
  '<?'        {TokenSetMin _}
  '$?'        {TokenSetSize _}

  -- -- map --
  '<+>'       {TokenMapPlus _}
  '<->'       {TokenMapMinus _}
  '<*>'       {TokenMapTimes _}
  '</>'       {TokenMapDiv _}
  '<%>'       {TokenMapMod _}

  -- -- bool --
  and         {TokenAnd _}
  or          {TokenOr _}
  not         {TokenNot _}

  -- -- relational --
  '<'         {TokenLT _}
  '<='        {TokenLE _}
  '>'         {TokenGT _}
  '>='        {TokenGE _}
  '=='        {TokenEQ _}
  '/='        {TokenNE _}
  '@'         {TokenAt _}

  -- control statements --
  if          {TokenIf _}
  else        {TokenElse _}
  for         {TokenFor _}
  min         {TokenMin _}
  max         {TokenMax _}
  repeat      {TokenRepeat _}
  while       {TokenWhile _}
  do          {TokenDo _}

  -- IO functions --
  scan        {TokenScan _}
  print       {TokenPrint _}
  println     {TokenPrintln _}

  -- variables --
  num         {TokenInt _ _}
  str         {TokenString _ _}
  id          {TokenIdent _ _}

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

Program
  : program Inst                            {Program $2}

Exp
  : Exp '+'   Exp                           {Binary Plus  $1 $3}
  | Exp '-'   Exp                           {Binary Minus $1 $3}
  | Exp '*'   Exp                           {Binary Times $1 $3}
  | Exp '/'   Exp                           {Binary Div   $1 $3}
  | Exp '%'   Exp                           {Binary Mod   $1 $3}

  | Exp '++'  Exp                           {Binary SetUnion $1 $3}
  | Exp '\\'  Exp                           {Binary SetMinus $1 $3}
  | Exp '><'  Exp                           {Binary SetInter $1 $3}

  | Exp '<+>' Exp                           {Binary MapPlus  $1 $3}
  | Exp '<->' Exp                           {Binary MapMinus $1 $3}
  | Exp '<*>' Exp                           {Binary MapTimes $1 $3}
  | Exp '</>' Exp                           {Binary MapDiv   $1 $3}
  | Exp '<%>' Exp                           {Binary MapMod   $1 $3}

  | Exp '<'   Exp                           {Binary CompLT $1 $3}
  | Exp '<='  Exp                           {Binary CompLE $1 $3}
  | Exp '>'   Exp                           {Binary CompGT $1 $3}
  | Exp '>='  Exp                           {Binary CompGE $1 $3}
  | Exp '=='  Exp                           {Binary CompEQ $1 $3}
  | Exp '/='  Exp                           {Binary CompNE $1 $3}
  | Exp '@'   Exp                           {Binary CompAt $1 $3}

  | Exp and   Exp                           {Binary And $1 $3}
  | Exp or    Exp                           {Binary Or  $1 $3}

  |     '>?'  Exp                           {Unary  SetMax   $2}
  |     '<?'  Exp                           {Unary  SetMin   $2}
  |     '$?'  Exp                           {Unary  SetSize  $2}
  |     not   Exp                           {Unary  Not      $2}
  |     '-'   Exp %prec NEG                 {Unary  Negative $2}

  | '(' Exp ')'                             {$2}
  | '{' Conts '}'                           {Set $2}
  | num                                     {IntConst (extract' $1)}
  | Bool                                    {BoolConst $1}
  | str                                     {StrConst (extract $1)}
  | id                                      {Var (extract $1)}

Bool
  : true                                    {True}
  | false                                   {False}

Conts
  : Exp ',' Conts                           {$1 : $3}
  | Exp                                     {[$1]}
  |                                         {[]}

Insts
  : Inst ';' Insts                          {$1 : $3}
  |                                         {[]}

Inst
  : id '=' Exp                              {Assign (extract $1) $3}

  | '{' using Declares in Insts '}'         {Block $3 $5}
  | '{' Insts '}'                           {Block [] $2}

  | scan id                                 {Scan    (extract $2)}
  | print Conts                             {Print   $2}
  | println Conts                           {Print  ($2 ++ [StrConst "\n"])}

  | if '(' Exp ')' Inst else Inst           {If  $3 $5 (Just $7)}
  | if '(' Exp ')' Inst                     {If  $3 $5 Nothing}

  | repeat Inst while '(' Exp ')' do Inst   {RWD (Just $2) $5 (Just $8)}
  | while '(' Exp ')' do Inst               {RWD Nothing   $3 (Just $6)}
  | repeat Inst while '(' Exp ')'           {RWD (Just $2) $5 Nothing}

  | for id Dir Exp do Inst                  {For (extract $2) $3 $4 $6}

Dir
  : min                                     {Min}
  | max                                     {Max}

Declares
  : Declare ';' Declares                    {$1 : $3}
  | Declare ';'                             {[$1]}

Declare : Type Variables                    {Declare $1 $2}

Type
  : bool                                    {BoolType}
  | int                                     {IntType}
  | set                                     {SetType}

Variables
  : Var ',' Variables                       {$1 : $3}
  | Var                                     {[$1]}
Var
  : id                                      {extract $1}

{

extract :: Token -> String
extract (TokenString s _) = s
extract (TokenIdent s _)  = s

extract' :: Token -> Int
extract' (TokenInt n _) = n

parseError :: [Token] -> a
parseError l = case l of
    [] -> error $ "Unexpected EOF"
    _  -> error $ "Unexpected " ++ show (head l)

parser :: String -> IO ()
parser text = do
    let toks = alexScanTokens text
    if any isTokenError toks
        then mapM_ printError $ filter isTokenError toks
        else putStrLn . show' . parsr $ toks

}
