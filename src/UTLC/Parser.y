{
module UTLC.Parser where

import UTLC.Lexer
import UTLC.Syntax
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    if       { IF }
    then     { THEN }
    else     { ELSE }
    "lambda" { LAMBDA }
    '('      { LPAREN }
    ')'      { RPAREN }
    '{'      { LBRACE }
    '}'      { RBRACE }
    '.'      { DOT }
    ';'      { SEMICOLON }
    let      { LET }
    in       { IN }
    true     { TRUE }
    false    { FALSE }
    succ     { SUCC }
    pred     { PRED }
    iszero   { ISZERO }
    var      { VAR $$ }
    string   { STRING $$ }
    number   { NUMBER $$ }

%%

Expr : Term             { Eval $1 }

Term : Atom             { $1 }

Atom : true             { TmTrue }
     | false            { TmFalse }
     | number           { let f n = case n of
                                      0 -> TmZero
                                      _ -> TmSucc (f $ n - 1)
                          in f $1 }
     | string           { TmString $1 }
     | '(' Term ')'     { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}