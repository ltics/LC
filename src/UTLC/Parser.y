{
module UTLC.Parser where

import UTLC.Lexer
import UTLC.Syntax
import UTLC.Context
import Data.List
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    lambda   { LAMBDA }
    '('      { LPAREN }
    ')'      { RPAREN }
    '.'      { DOT }
    var      { VAR $$ }

%%

Term : lambda var '.' Term          { unsafePerformIO $ do
                                        addName $2
                                        $4 `seq` return $ TmAbs $2 $4 }
     | var                          { unsafePerformIO $ do
                                        ctx <- readIORef globalContext
                                        idx <- name2index $1
                                        return $ TmVar idx (length ctx) }
     -- if you want hold global state and keep track on that, you need strict semantic instead of lazy semantic
     | Term Term                    { $1 `seq` $2 `seq` TmApp $1 $2 }
     | '(' Term ')'                 { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Term
parseExpr = expr . scanTokens
}