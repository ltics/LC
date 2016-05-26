{
module UTLC.Lexer where

import UTLC.Syntax
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
       "if"                        { \_ -> IF }
       "then"                      { \_ -> THEN }
       "else"                      { \_ -> ELSE }
       "λ"                         { \_ -> LAMBDA }
       "("                         { \_ -> LPAREN }
       ")"                         { \_ -> RPAREN }
       "{"                         { \_ -> LBRACE }
       "}"                         { \_ -> RBRACE }
       "."                         { \_ -> DOT }
       ";"                         { \_ -> SEMICOLON }
       "let"                       { \_ -> LET }
       "in"                        { \_ -> IN }
       "true"                      { \_ -> TRUE }
       "false"                     { \_ -> FALSE }
       "succ"                      { \_ -> SUCC }
       "pred"                      { \_ -> PRED }
       "iszero"                    { \_ -> ISZERO }
       $lower $chars*              { \s -> VAR s }
       \"[^\"]*\"                  { \s -> STRING ((tail . init) s) }
       $digit+                     { \s -> NUMBER (read s) }
       "-" $digit+                 { \s -> NUMBER (read s) }

{
data Token = IF
           | THEN
           | ELSE
           | LAMBDA
           | LPAREN
           | RPAREN
           | LBRACE
           | RBRACE
           | DOT
           | SEMICOLON
           | LET
           | IN
           | TRUE
           | FALSE
           | SUCC
           | PRED
           | ISZERO
           | VAR Name
           | NUMBER Int
           | STRING String
           deriving(Eq, Show)

scanTokens = alexScanTokens
}