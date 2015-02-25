{
module Parser
( parser
, parsr
, tp
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
  : Exp '+'   Exp                           {Binary (Plus  (tp $2)) $1 $3}
  | Exp '-'   Exp                           {Binary (Minus (tp $2)) $1 $3}
  | Exp '*'   Exp                           {Binary (Times (tp $2)) $1 $3}
  | Exp '/'   Exp                           {Binary (Div   (tp $2)) $1 $3}
  | Exp '%'   Exp                           {Binary (Mod   (tp $2)) $1 $3}

  | Exp '++'  Exp                           {Binary (SetUnion (tp $2)) $1 $3}
  | Exp '\\'  Exp                           {Binary (SetMinus (tp $2)) $1 $3}
  | Exp '><'  Exp                           {Binary (SetInter (tp $2)) $1 $3}

  | Exp '<+>' Exp                           {Binary (MapPlus  (tp $2)) $1 $3}
  | Exp '<->' Exp                           {Binary (MapMinus (tp $2)) $1 $3}
  | Exp '<*>' Exp                           {Binary (MapTimes (tp $2)) $1 $3}
  | Exp '</>' Exp                           {Binary (MapDiv   (tp $2)) $1 $3}
  | Exp '<%>' Exp                           {Binary (MapMod   (tp $2)) $1 $3}

  | Exp '<'   Exp                           {Binary (CompLT (tp $2)) $1 $3}
  | Exp '<='  Exp                           {Binary (CompLE (tp $2)) $1 $3}
  | Exp '>'   Exp                           {Binary (CompGT (tp $2)) $1 $3}
  | Exp '>='  Exp                           {Binary (CompGE (tp $2)) $1 $3}
  | Exp '=='  Exp                           {Binary (CompEQ (tp $2)) $1 $3}
  | Exp '/='  Exp                           {Binary (CompNE (tp $2)) $1 $3}
  | Exp '@'   Exp                           {Binary (CompAt (tp $2)) $1 $3}

  | Exp and   Exp                           {Binary (And (tp $2)) $1 $3}
  | Exp or    Exp                           {Binary (Or  (tp $2)) $1 $3}

  |     '>?'  Exp                           {Unary  (SetMax   (tp $1)) $2}
  |     '<?'  Exp                           {Unary  (SetMin   (tp $1)) $2}
  |     '$?'  Exp                           {Unary  (SetSize  (tp $1)) $2}
  |     not   Exp                           {Unary  (Not      (tp $1)) $2}
  |     '-'   Exp %prec NEG                 {Unary  (Negative (tp $1)) $2}

  | '(' Exp ')'                             {$2}
  | '{' Conts '}'                           {Set $2 (tp $1)}
  | num                                     {IntConst (extract' $1)}
  | Bool                                    {BoolConst $1}
  | str                                     {StrConst (extract $1)}
  | Name                                    {Var $1}

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
  : Name '=' Exp                            {Assign $1 $3 ((\(Id _ p) -> p) $1)}

  | '{' using Declares in Insts '}'         {Block $3 $5 (tp $1)}
  | '{' Insts '}'                           {Block [] $2 (tp $1)}

  | scan Name                               {Scan    $2                     (tp $1)}
  | print Conts                             {Print   $2                     (tp $1)}
  | println Conts                           {Print  ($2 ++ [StrConst "\n"]) (tp $1)}

  | if '(' Exp ')' Inst else Inst           {If  $3 $5 (Just $7) (tp $1)}
  | if '(' Exp ')' Inst                     {If  $3 $5 Nothing   (tp $1)}

  | repeat Inst while '(' Exp ')' do Inst   {RWD (Just $2) $5 (Just $8) (tp $3)}
  | while '(' Exp ')' do Inst               {RWD Nothing   $3 (Just $6) (tp $1)}
  | repeat Inst while '(' Exp ')'           {RWD (Just $2) $5 Nothing   (tp $3)}

  | for Name Dir Exp do Inst                {For $2 $3 $4 $6 (tp $1)}

Dir
  : min                                     {Min}
  | max                                     {Max}

Declares
  : Declare ';' Declares                    {$1 : $3}
  | Declare ';'                             {[$1]}

Declare : Type Names                        {Declare $1 $2}

Type
  : bool                                    {BoolType}
  | int                                     {IntType}
  | set                                     {SetType}

Names
  : Name ',' Names                          {$1 : $3}
  | Name                                    {[$1]}
Name
  : id                                      {Id (extract $1) (tp $1)}

{

tp :: Token -> Pos
tp = token_posn

extract :: Token -> String
extract (TokenString s _) = s
extract (TokenIdent s _)  = s

extract' :: Token -> Int
extract' (TokenInt n _) = n

parseError :: [Token] -> a
parseError l = case l of
  [] -> error $ "Unexpected EOF"
  _  -> error $ "Unexpected " ++ show (head l)

parser :: String -> String -> IO ()
parser text name = do
  putStrLn $ "Parser (" ++ name ++ "):\n"
  let toks = alexScanTokens text
  if any isTokenError toks
    then mapM_ printError $ filter isTokenError toks
    else putStrLn . show . parsr $ toks
  putStrLn ""

}
