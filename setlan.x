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
    program                             { tok (\p s -> TokProgram p) }
    using                               { tok (\p s -> TokUsing p) }
    in                                  { tok (\p s -> TokIn p) }
    int                                 { tok (\p s -> TokIntT p) }
    $digit+                             { tok (\p s -> TokInt (read s) p ) }
    bool                                { tok (\p s -> TokBoolT p) }
    [Tt]rue                             { tok (\p s -> TokBool True p) }
    [Ff]alse                            { tok (\p s -> TokBool False p) }
    set                                 { tok (\p s -> TokSetT p) }
    [\{]                                { tok (\p s -> TokCurlyOpen p) }
    [\}]                                { tok (\p s -> TokCurlyClose p) }
    [\(]                                { tok (\p s -> TokParenOpen p) }
    [\)]                                { tok (\p s -> TokParenClose p) }
    [\,]                                { tok (\p s -> TokComma p) }
    [\;]                                { tok (\p s -> TokSemicolon p) }
    [\:]                                { tok (\p s -> TokColon p) }
    [=]                                 { tok (\p s -> TokAssign p) }
    [\+\-\*\/\%]                        { tok (\p s -> TokIntOp s p) }
    (\+\+)|(\\)|(\>\<)                  { tok (\p s -> TokSetBinOp s p) }
    [\>\<\$]\?                          { tok (\p s -> TokSetUnOp s p) }
    \<[\+\-\*\/\%]\>                    { tok (\p s -> TokMapOp s p) }
    (and)|(or)|(not)                    { tok (\p s -> TokBoolOp s p) }
    (\<=?)|(\>=?)|(==)|(\/=)|@          { tok (\p s -> TokRelOp s p) }
    if                                  { tok (\p s -> TokIf p) }
    then                                { tok (\p s -> TokThen p) }
    else                                { tok (\p s -> TokElse p) }
    for                                 { tok (\p s -> TokFor p) }
    min                                 { tok (\p s -> TokMin p) }
    max                                 { tok (\p s -> TokMax p) }
    repeat                              { tok (\p s -> TokRepeat p) }
    while                               { tok (\p s -> TokWhile p) }
    do                                  { tok (\p s -> TokDo p) }
    def                                 { tok (\p s -> TokDef p) }
    \-\>                                { tok (\p s -> TokArrow p) }
    return                              { tok (\p s -> TokReturn p) }
    scan                                { tok (\p s -> TokScan p) }
    print                               { tok (\p s -> TokPrint p) }
    println                             { tok (\p s -> TokPrintln p) }
    \"[^\"]*\"                          { tok (\p s -> TokString (read s) p) }
    $alpha [$alpha $digit \_ \']*       { tok (\p s -> TokVar s p) }

{
tok f p s = f p s

data Token = TokProgram                 AlexPosn
           | TokUsing                   AlexPosn
           | TokIn                      AlexPosn
           | TokIntT                    AlexPosn
           | TokInt         Int         AlexPosn
           | TokBoolT                   AlexPosn
           | TokBool        Bool        AlexPosn
           | TokSetT                    AlexPosn
           | TokCurlyOpen               AlexPosn
           | TokCurlyClose              AlexPosn
           | TokParenOpen               AlexPosn
           | TokParenClose              AlexPosn
           | TokComma                   AlexPosn
           | TokSemicolon               AlexPosn
           | TokColon                   AlexPosn
           | TokAssign                  AlexPosn
           | TokIntOp       String      AlexPosn
           | TokSetBinOp    String      AlexPosn
           | TokSetUnOp     String      AlexPosn
           | TokMapOp       String      AlexPosn
           | TokBoolOp      String      AlexPosn
           | TokRelOp       String      AlexPosn
           | TokIf                      AlexPosn
           | TokThen                    AlexPosn
           | TokElse                    AlexPosn
           | TokFor                     AlexPosn
           | TokMin                     AlexPosn
           | TokMax                     AlexPosn
           | TokRepeat                  AlexPosn
           | TokWhile                   AlexPosn
           | TokDo                      AlexPosn
           | TokDef                     AlexPosn
           | TokArrow                   AlexPosn
           | TokReturn                  AlexPosn
           | TokScan                    AlexPosn
           | TokPrint                   AlexPosn
           | TokPrintln                 AlexPosn
           | TokString      String      AlexPosn
           | TokVar         String      AlexPosn
           deriving (Eq, Show)

token_posn(TokProgram p) = p
token_posn(TokUsing p) = p
token_posn(TokIn p) = p
token_posn(TokIntT p) = p
token_posn(TokInt _ p) = p
token_posn(TokBoolT p) = p
token_posn(TokBool _ p) = p
token_posn(TokSetT p) = p
token_posn(TokCurlyOpen p) = p
token_posn(TokCurlyClose p) = p
token_posn(TokParenOpen p) = p
token_posn(TokParenClose p) = p
token_posn(TokComma p) = p
token_posn(TokSemicolon p) = p
token_posn(TokColon p) = p
token_posn(TokAssign p) = p
token_posn(TokIntOp _ p) = p
token_posn(TokSetBinOp _ p) = p
token_posn(TokSetUnOp _ p) = p
token_posn(TokMapOp _ p) = p
token_posn(TokBoolOp _ p) = p
token_posn(TokRelOp _ p) = p
token_posn(TokIf p) = p
token_posn(TokThen p) = p
token_posn(TokElse p) = p
token_posn(TokFor p) = p
token_posn(TokMin p) = p
token_posn(TokMax p) = p
token_posn(TokRepeat p) = p
token_posn(TokWhile p) = p
token_posn(TokDo p) = p
token_posn(TokDef p) = p
token_posn(TokArrow p) = p
token_posn(TokReturn p) = p
token_posn(TokScan p) = p
token_posn(TokPrint p) = p
token_posn(TokPrintln p) = p
token_posn(TokString _ p) = p
token_posn(TokVar _ p) = p

fromFile::FilePath -> IO()
fromFile filename = do
    putStrLn ("\n<<Tokenizing " ++ filename  ++ ">>\n")
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
