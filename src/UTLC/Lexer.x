{
module UTLC.Lexer where

import UTLC.Context
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
       $lower $chars*              { \s -> VAR s }

{
data Token = LAMBDA
           | LPAREN
           | RPAREN
           | DOT
           | VAR Name
           deriving(Eq, Show)

scanTokens = alexScanTokens
}