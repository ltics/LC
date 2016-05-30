{
module LetRec.Lexer where

import LetRec.Syntax
}

%wrapper "basic"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$chars = [$lower $upper $digit]
$eol = [\n]

tokens :-
       $eol                        ;
       $white+                     ;
       ";;".*                      ; --comments
       "λ"                         { \_ -> LAMBDA }
       "("                         { \_ -> LPAREN }
       ")"                         { \_ -> RPAREN }
       "."                         { \_ -> DOT }
       "="                         { \_ -> IFF }
       if                          { \_ -> IF }
       then                        { \_ -> THEN }
       else                        { \_ -> ELSE }
       in                          { \_ -> IN }
       let                         { \_ -> LET }
       letrec                      { \_ -> LETREC }
       "zero?"                     { \_ -> ISZERO }
       $lower $chars*              { \s -> VAR s }
       $digit+                     { \s -> NUM (read s) }
       "-" $digit+                 { \s -> NUM (read s) }

{
data Token = LAMBDA
           | LPAREN
           | RPAREN
           | DOT
           | IFF
           | IF
           | THEN
           | ELSE
           | IN
           | LET
           | LETREC
           | ISZERO
           | VAR Name
           | NUM Int
           deriving(Eq, Show)

scanTokens = alexScanTokens
}