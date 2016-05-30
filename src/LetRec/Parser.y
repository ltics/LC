{
module LetRec.Parser where

import LetRec.Lexer
import LetRec.Syntax
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    lambda   { LAMBDA }
    '('      { LPAREN }
    ')'      { RPAREN }
    '.'      { DOT }
    iff      { IFF }
    if       { IF }
    then     { THEN }
    else     { ELSE }
    in       { IN }
    let      { LET }
    letrec   { LETREC }
    iszero   { ISZERO }
    binop    { BINOP $$ }
    var      { VAR $$ }
    num      { NUM $$ }

%%

Expr : if Expr then Expr else Expr     { EIf $2 $4 $6 }
     | let var iff Expr in Expr        { ELet $2 $4 $6 }
     | letrec var var iff Expr in Expr { ELetRec $2 $3 $5 $7 }
     | lambda var '.' Expr             { EAbs $2 $4 }
     | iszero Expr                     { EIsZero $2 }
     | Expr binop Expr                 { let op = case $2 of
                                                    "+" -> Add
                                                    "-" -> Sub
                                                    "*" -> Mul
                                                    "/" -> Div
                                         in EBinOp op $1 $3 }
     | App                             { $1 }

App : Atom                             { $1 }
    | App Atom                         { EApp $1 $2 }

Atom : var                             { EVar $1 }
     | num                             { ENum $1 }
     | '(' Expr ')'                    { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}