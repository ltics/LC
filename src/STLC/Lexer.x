{
module STLC.Lexer where

import STLC.Context
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
       "→"                         { \_ -> ARROW }
       "("                         { \_ -> LPAREN }
       ")"                         { \_ -> RPAREN }
       "."                         { \_ -> DOT }
       ":"                         { \_ -> COLON }
       "="                         { \_ -> IFF }
       "true"                      { \_ -> TRUE }
       "false"                     { \_ -> FALSE }
       if                          { \_ -> IF }
       then                        { \_ -> THEN }
       else                        { \_ -> ELSE }
       in                          { \_ -> IN }
       let                         { \_ -> LET }
       "0"                         { \_ -> ZERO }
       "succ"                      { \_ -> SUCC }
       "pred"                      { \_ -> PRED }
       "zero?"                     { \_ -> ISZERO }
       "fix"                       { \_ -> FIX }
       "Int"                       { \_ -> INT }
       "Bool"                      { \_ -> BOOL }
       $lower $chars*              { \s -> VAR s }

{
data Token = LAMBDA
           | ARROW
           | LPAREN
           | RPAREN
           | DOT
           | COLON
           | IFF
           | TRUE
           | FALSE
           | IF
           | THEN
           | ELSE
           | IN
           | LET
           | ZERO
           | SUCC
           | PRED
           | ISZERO
           | FIX
           | INT
           | BOOL
           | VAR Name
           deriving(Eq, Show)

scanTokens = alexScanTokens
}