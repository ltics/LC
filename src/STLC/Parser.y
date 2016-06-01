{
module STLC.Parser where

import STLC.Lexer
import STLC.Syntax
import STLC.Type
import STLC.Context
import Data.List
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    lambda   { LAMBDA }
    arrow    { ARROW }
    '('      { LPAREN }
    ')'      { RPAREN }
    '.'      { DOT }
    ':'      { COLON }
    iff      { IFF }
    if       { IF }
    true     { TRUE }
    false    { FALSE }
    then     { THEN }
    else     { ELSE }
    in       { IN }
    let      { LET }
    letrec   { LETREC }
    zero     { ZERO }
    succ     { SUCC }
    pred     { PRED }
    iszero   { ISZERO }
    fix      { FIX }
    int      { INT }
    bool     { BOOL }
    var      { VAR $$ }

%%

Term : lambda var ':' Type '.' Term { unsafePerformIO $ do
                                        addName $2 $4
                                        $6 `seq` return $ TmAbs $2 $4 $6 }
     | if Term then Term else Term  { TmIf $2 $4 $6 }
     | iszero Term                  { TmIsZero $2 }
     | succ Term                    { TmSucc $2 }
     | pred Term                    { TmPred $2 }
     | let var iff Term in Term     { unsafePerformIO $ do
                                        addName $2 TUnit
                                        $6 `seq` $4 `seq` return $ TmLet $2 $4 $6 }
     | fix Term                     { TmFix $2 }
     | App                          { $1 }

App : Atom                          { $1 }
    -- if you want hold global state and keep track on that, you need strict semantic instead of lazy semantic
    | App Atom                      { $1 `seq` $2 `seq` TmApp $1 $2 }

Atom : var                          { unsafePerformIO $ do
                                      ctx <- readIORef globalContext
                                      idx <- name2index $1
                                      return $ TmVar idx (length ctx) }
     | zero                         { TmZero }
     | true                         { TmTrue }
     | false                        { TmFalse }
     | '(' Term ')'                 { $2 }

Type : AtomType                     { $1 }
     | AtomType arrow Type          { $1 :~> $3 }

AtomType : int                      { TInt }
         | bool                     { TBool }
         | '(' Type ')'             { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Term
parseExpr = expr . scanTokens
}