{
module Main (Token(..), AlexPosn(..), alexScanTokens, token_posn, main) where
import System.Environment
import System.IO
}

-- Positional wrapper
%wrapper "posn"

-- Macros
$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

-- Each right-hand side has type :: AlexPosn -> String -> Token
tokens :-
    $white+                             ;
    "#".*                               ;
    program                             { tok (\p s -> TokenProgram p) }
    using                               { tok (\p s -> TokenUsing p) }
    in                                  { tok (\p s -> TokenIn p) }
    int                                 { tok (\p s -> TokenIntT p) }
    $digit+                             { tok (\p s -> TokenInt (read s) p ) }
    bool                                { tok (\p s -> TokenBoolT p) }
    [Tt]rue                             { tok (\p s -> TokenBool True p) }
    [Ff]alse                            { tok (\p s -> TokenBool False p) }
    set                                 { tok (\p s -> TokenSetT p) }
    [\{]                                { tok (\p s -> TokenCurlyOpen p) }
    [\}]                                { tok (\p s -> TokenCurlyClose p) }
    [\(]                                { tok (\p s -> TokenParenOpen p) }
    [\)]                                { tok (\p s -> TokenParenClose p) }
    [\,]                                { tok (\p s -> TokenComma p) }
    [\;]                                { tok (\p s -> TokenSemicolon p) }
    [\:]                                { tok (\p s -> TokenColon p) }
    [=]                                 { tok (\p s -> TokenAssign p) }
    [\+\-\*\/\%]                        { tok (\p s -> TokenIntOp s p) }
    (\+\+)|(\\)|(\>\<)                  { tok (\p s -> TokenSetBinOp s p) }
    [\>\<\$]\?                          { tok (\p s -> TokenSetUnOp s p) }
    \<[\+\-\*\/\%]\>                    { tok (\p s -> TokenMapOp s p) }
    (and)|(or)|(not)                    { tok (\p s -> TokenBoolOp s p) }
    (\<=?)|(\>=?)|(==)|(\/=)|@          { tok (\p s -> TokenRelOp s p) }
    if                                  { tok (\p s -> TokenIf p) }
    then                                { tok (\p s -> TokenThen p) }
    else                                { tok (\p s -> TokenElse p) }
    for                                 { tok (\p s -> TokenFor p) }
    min                                 { tok (\p s -> TokenMin p) }
    max                                 { tok (\p s -> TokenMax p) }
    repeat                              { tok (\p s -> TokenRepeat p) }
    while                               { tok (\p s -> TokenWhile p) }
    do                                  { tok (\p s -> TokenDo p) }
    def                                 { tok (\p s -> TokenDef p) }
    \-\>                                { tok (\p s -> TokenArrow p) }
    return                              { tok (\p s -> TokenReturn p) }
    scan                                { tok (\p s -> TokenScan p) }
    print                               { tok (\p s -> TokenPrint p) }
    println                             { tok (\p s -> TokenPrintln p) }
    \"[^\"]*\"                          { tok (\p s -> TokenString (read s) p) }
    $alpha [$alpha $digit \_ \']*       { tok (\p s -> TokenVar s p) }

{
tok f p s = f (toPos p) s

toPos :: AlexPosn -> Pos
toPos (AlexPn _ line column) = Pos line column

data Pos =
    Pos Int Int
    deriving (Eq, Show)

data Token = TokenProgram                 Pos
           | TokenUsing                   Pos
           | TokenIn                      Pos
           | TokenIntT                    Pos
           | TokenInt         Int         Pos
           | TokenBoolT                   Pos
           | TokenBool        Bool        Pos
           | TokenSetT                    Pos
           | TokenCurlyOpen               Pos
           | TokenCurlyClose              Pos
           | TokenParenOpen               Pos
           | TokenParenClose              Pos
           | TokenComma                   Pos
           | TokenSemicolon               Pos
           | TokenColon                   Pos
           | TokenAssign                  Pos
           | TokenIntOp       String      Pos
           | TokenSetBinOp    String      Pos
           | TokenSetUnOp     String      Pos
           | TokenMapOp       String      Pos
           | TokenBoolOp      String      Pos
           | TokenRelOp       String      Pos
           | TokenIf                      Pos
           | TokenThen                    Pos
           | TokenElse                    Pos
           | TokenFor                     Pos
           | TokenMin                     Pos
           | TokenMax                     Pos
           | TokenRepeat                  Pos
           | TokenWhile                   Pos
           | TokenDo                      Pos
           | TokenDef                     Pos
           | TokenArrow                   Pos
           | TokenReturn                  Pos
           | TokenScan                    Pos
           | TokenPrint                   Pos
           | TokenPrintln                 Pos
           | TokenString      String      Pos
           | TokenVar         String      Pos
           deriving (Eq, Show)

token_posn(TokenProgram p) = p
token_posn(TokenUsing p) = p
token_posn(TokenIn p) = p
token_posn(TokenIntT p) = p
token_posn(TokenInt _ p) = p
token_posn(TokenBoolT p) = p
token_posn(TokenBool _ p) = p
token_posn(TokenSetT p) = p
token_posn(TokenCurlyOpen p) = p
token_posn(TokenCurlyClose p) = p
token_posn(TokenParenOpen p) = p
token_posn(TokenParenClose p) = p
token_posn(TokenComma p) = p
token_posn(TokenSemicolon p) = p
token_posn(TokenColon p) = p
token_posn(TokenAssign p) = p
token_posn(TokenIntOp _ p) = p
token_posn(TokenSetBinOp _ p) = p
token_posn(TokenSetUnOp _ p) = p
token_posn(TokenMapOp _ p) = p
token_posn(TokenBoolOp _ p) = p
token_posn(TokenRelOp _ p) = p
token_posn(TokenIf p) = p
token_posn(TokenThen p) = p
token_posn(TokenElse p) = p
token_posn(TokenFor p) = p
token_posn(TokenMin p) = p
token_posn(TokenMax p) = p
token_posn(TokenRepeat p) = p
token_posn(TokenWhile p) = p
token_posn(TokenDo p) = p
token_posn(TokenDef p) = p
token_posn(TokenArrow p) = p
token_posn(TokenReturn p) = p
token_posn(TokenScan p) = p
token_posn(TokenPrint p) = p
token_posn(TokenPrintln p) = p
token_posn(TokenString _ p) = p
token_posn(TokenVar _ p) = p

fromFile::FilePath -> IO()
fromFile filename = do
    handle <- openFile (filename) ReadMode
    s <- hGetContents handle
    let toks = alexScanTokens s
    mapM_ print toks

main::IO ()
main = do
    let s = ""
    args <- getArgs
    if length args == 0
        then do
            s <- getContents
            let toks = alexScanTokens s
            mapM_ print toks
        else do
            mapM_ fromFile args
}
