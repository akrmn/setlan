--Revisa esto:
--    https://leanpub.com/alexandhappy/read#leanpub-auto-a-basic-lexer
--explica más o menos qué hace Alex. No está completo pero por ahora sirve.

{
module Main (Token(..), AlexPosn(..), alexScanTokens, token_posn, main) where
import System.Environment
}

--Este wrapper devuelve las posiciones de los tokens, que es una de las cosas
--que piden.
%wrapper "posn"


--Estos son los macros
$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

--Aquí vienen los tokens, que se parecen un poco a las producciones de las
--gramáticas de Ascander.
tokens :-
    $white+                           ;
    "#".*                             ;
    [Pp]rogram                        { tok (\p s -> TokProgram p) }
    [Uu]sing                          { tok (\p s -> TokUsing p) }
    [Ii]n                             { tok (\p s -> TokIn p) }

    [Ii]nt                            { tok (\p s -> TokIntT p) }

--TokInt es un poco distinto, porque guarda el valor como otro argumento
    $digit+                           { tok (\p s -> TokInt p (read s)) }

    [Bb]ool                           { tok (\p s -> TokBoolT p) }

--TokBool se parece a TokInt
    ([Tt]rue)|([Ff]alse)              { tok (\p s -> TokBool p (read s)) }

    [Ss]et                            { tok (\p s -> TokSetT p) }
    [\{]                              { tok (\p s -> TokCurlyOpen p) }
    [\}]                              { tok (\p s -> TokCurlyClose p) }

    [\,]                              { tok (\p s -> TokComma p) }
    [\;]                              { tok (\p s -> TokSemicolon p) }

    [\=\+\-\*\/\(\)]                  { tok (\p s -> TokSym p (head s)) }
    $alpha [$alpha $digit \_ \']*     { tok (\p s -> TokVar p s) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers: (no sé qué hace esto, venía en el ejemplo de Alex .-.)
tok f p s = f p s

-- The token type:
data Token = TokProgram     AlexPosn
           | TokUsing       AlexPosn
           | TokIn          AlexPosn
           | TokIntT        AlexPosn
--Aquí ves que TokInt es distinto, tiene un segundo argumento de tipo Int
           | TokInt         AlexPosn Int
           | TokBoolT       AlexPosn
           | TokBool        AlexPosn Bool
           | TokSetT        AlexPosn
           | TokCurlyOpen   AlexPosn
           | TokCurlyClose  AlexPosn
           | TokComma       AlexPosn
           | TokSemicolon   AlexPosn
           | TokSym         AlexPosn Char
           | TokVar         AlexPosn String
           deriving (Eq,Show)

token_posn (TokProgram p) = p
token_posn (TokUsing p) = p
token_posn (TokIn p) = p
token_posn (TokIntT p) = p
token_posn (TokInt p _) = p
token_posn (TokBoolT p) = p
token_posn (TokBool p _) = p
token_posn (TokSetT p) = p
token_posn (TokCurlyOpen p) = p
token_posn (TokCurlyClose p) = p
token_posn (TokComma p) = p
token_posn (TokSemicolon p) = p
token_posn (TokSym p _) = p
token_posn (TokVar p _) = p

main::IO ()
main = do
    {-|
        Me gustaría que el Lexer funcionara tanto después de un pipe (leyendo
        de stdin, que lo hace con getContents), como con un argumento con el
        nombre del archivo (usando getArgs y luego algo que lea el archivo).

        Por ahora el programa se usa así:
            cat <archivo.stl> | ./Tokens
    -}
    s <- getContents
    let toks = alexScanTokens s

    {-|
        También me gustaría cambiar el print para que se parezca más al de
        ellos, aunque no va a ser idéntico por cómo funciona Haskell
    -}
    mapM_ print toks
}
