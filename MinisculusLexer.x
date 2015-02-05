{

{-
    Minisculus Lexer
    ================
    James Sullivan - 10095183
    <sullivan.james.f@gmail.com>
    CPSC 411 - W2015 - University of Calgary

    Usage
    =====
    cat $filename | runhaskell $lexername.hs

    Tokenize the infile according to the Minisculus syntax, writing the
    token list to stdout. If the lexing fails, an error message is
    displayed.

    Comment Precedence
    ==================
    Comments are removed in the following order.
        1) An inline comment block strips all characters up to the
            first occurance of newline. This effect applies regardless
            of whether or not the delimiter is within a comment block.
            IE, these take precedence over multiline blocks, and all
            characters between the delimiter and a newline are always
            ignored.
        2) A multiline block of the form "/* ... */" (possibly nested)
            ignores all characters that fall between the two boundaries,
            including the boundary characters themselves. The comment
            state is exited only when all "/*" delimiters have been
            matched with a "*/" delimiter (that do not fall after an
            inline comment delimiter).
                              
-}

module MinisculusLexer where

import Data.List
import MinisculusPos
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    <0> "%" [^\n]*              ;
    <comment> "%" [^\n]*        ;
    <0> "/*"                    { incLevel `andBegin` comment }
    <comment> "/*"              { incLevel }
    <comment> "*/"              { decLevel }
    <comment> . | \n            ;
    $white+                     ;
    <0> if                      { mkT T_If }
    <0> then                    { mkT T_Then }
    <0> while                   { mkT T_While }
    <0> do                      { mkT T_Do }
    <0> input                   { mkT T_Input }
    <0> else                    { mkT T_Else }
    <0> begin                   { mkT T_Begin }
    <0> end                     { mkT T_End }
    <0> write                   { mkT T_Write }
    <0> ":="                    { mkT T_Assign }
    <0> "+"                     { mkT T_Add }   
    <0> "-"                     { mkT T_Sub }
    <0> "*"                     { mkT T_Mul }
    <0> "/"                     { mkT T_Div }
    <0> "("                     { mkT T_LPar }
    <0> ")"                     { mkT T_RPar }
    <0> ";"                     { mkT T_Semicolon }
    <0> $alpha [$alpha $digit]* { identifier } 
    <0> $digit+                 { number }
    <0> .                       { lexError }
{

{- Lexer Token definitions and main routines -}

-- Token Data Type.
data Token = Token Pos TokenClass deriving (Eq, Show)

data TokenClass  = T_Identifier String
            | T_Num Int
            | T_If
            | T_Then
            | T_While
            | T_Do
            | T_Input
            | T_Else
            | T_Begin
            | T_End
            | T_Write
            | T_Add
            | T_Assign
            | T_Sub
            | T_Mul
            | T_Div
            | T_LPar
            | T_RPar
            | T_Semicolon
            | T_EOF
            deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
    (p,_,_,_) <- alexGetInput
    return $ Token (toPos p) T_EOF

toPos :: AlexPosn -> Pos
toPos (AlexPn _ l c) = Pos (Line l) (Column c)

-- Extracts the token type, returning as an Alex Token datum.
mkT :: TokenClass -> AlexInput -> Int -> Alex Token 
mkT c (p, _, _, _) _ = return $
    Token (toPos p) c

-- Constructs a T_Identifier token from the currently parsed string.
identifier :: AlexInput -> Int -> Alex Token
identifier (p, _, _, inp) len = return $ 
    Token (toPos p) (T_Identifier (take len inp))

-- Constructs a T_Num token from the currently parsed number.
number :: AlexInput -> Int -> Alex Token
number (p, _, _, inp) len = return $
    Token (toPos p) (T_Num (read $ take len inp))

-- Monad state data type. We only need to keep track of comment depth.
data AlexUserState = AlexUserState { 
    commentDepth :: Int 
}

-- Initialize the comment depth to 0
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { commentDepth = 0 }

-- Set the comment depth to the given integer.
setCommentDepth :: Int -> Alex ()
setCommentDepth new = Alex $
    (\st -> Right (st{alex_ust=(alex_ust st){commentDepth = new}}, ()))

-- Return the current comment depth
getCommentDepth :: Alex Int
getCommentDepth = Alex $
    (\st@AlexState{alex_ust=ust} -> Right (st, commentDepth ust))

-- Increases the comment depth by 1.
incLevel :: AlexInput -> Int -> Alex Token 
incLevel a d =
    do  cdepth <- getCommentDepth    -- Get comment depth
        setCommentDepth (cdepth + 1) -- Increment comment depth
        skip a d

-- Decreases the comment depth by 1. If this goes below 1, then
-- the '0' start code is entered and the comment depth is set to 0.
decLevel :: AlexInput -> Int -> Alex Token 
decLevel a d =
    do  cdepth <- getCommentDepth    -- Get comment depth
        setCommentDepth (cdepth - 1) -- Decrement comment depth
        if (cdepth <= 1) then do 
            alexSetStartCode 0       -- Reset start code
            setCommentDepth  0       -- Reset comment depth
            skip a d 
        else 
            skip a d

-- Raises an alexError with a useful error message indicating the
-- line and the column of the error, as well as printing the invalid
-- string.
lexError :: AlexInput -> Int -> Alex Token 
lexError (p, _, _, inp) len = alexError $ 
    "unexpected token '" ++ (take len inp) ++ "' at " ++ showPos p

-- Prints a string representation of the line and column of the given
-- AlexPn type.
showPos (AlexPn _ l c) = show l ++ ":" ++ show c

-- Tokenize the input string according to the above token definitions.
-- On failure, 
gettokens str = runAlex str $ do
    let loop tokPairs = do 
        tok <- alexMonadScan; 
        case tok of
            Token _ T_EOF   -> return (reverse tokPairs)
            _               -> do loop (tok:tokPairs)
    loop []

tokenUnwrap :: [Token] -> [(Pos, TokenClass)]
tokenUnwrap = map (\(Token p t) -> (p,t))

tokenize = do
    s <- getContents
    let l = gettokens s
    case l of
        Left s  -> error s
        Right t -> putStrLn $ foldr1 (\s1 s2 -> s1 ++ "  " ++ s2) $
                        map show t

}

