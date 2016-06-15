{
module PI.Parser where

import PI.Lexer
import PI.PI
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    forall    { FORALL }
    lambda    { LAMBDA }
    arrow     { ARROW }
    '('       { LPAREN }
    ')'       { RPAREN }
    '['       { LBRACKET }
    ']'       { RBRACKET }
    '.'       { DOT }
    '::'      { DOUBLECOLON }
    ';'       { SEMICOLON }
    '='       { IFF }
    '*'       { STAR }
    '_'       { WILDCARD }
    let       { LET }
    in        { IN }
    var       { VAR $$ }
    num      { NUM $$ }

%%

Expr : Lam                                         { $1 }
     | Pi                                          { $1 }
     | App                                         { $1 }
     | Let                                         { $1 }

Let : let BindStar ';' in Expr                     { eLets' $2 $5 }

BindStar : {- empty -}                             { [] }
         | Bind ';' BindStar                       { $1 : $3 }

Bind : BindH                                       { $1 }
     | BindR                                       { $1 }

NamebolStar : {- empty -}                          { [] }
           | var NamebolStar                       { $1 : $2 }

BindH : var '::' Type ';' var NamebolStar '=' Expr { if $1 /= $5
                                                     then error "bind name not match"
                                                     else ($1, $3, Just (matchH $3 $6 $8)) }

BindR : var ArgStar '::' Type                      { ($1, foldr addT $4 $2, Nothing) }
      | var ArgStar '::' Type '=' Expr             { ($1, foldr addT $4 $2, Just $ foldr addE $6 $2) }

Lam : lambda VarTypePlus arrow Expr                { foldr (uncurry Lam) $4 $2 }

Type : Expr                                        { $1 }

VarType : var '::' Type                            { ($1, $3) }
        | '(' VarType ')'                          { $2 }

VarTypePlus : VarType                              { [$1] }
            | VarType VarTypePlus                  { $1 : $2 }

ArgStar : {- empty -}                              { [] }
        | Arg ArgStar                              { $1 : $2 }

Arg : '(' VarType ')'                              { $2 }

PiArg : Arg                                        { $1 }
      | PiNoDep                                    { $1 }

PiNoDep : App                                      { ("_", $1) }

PiArrPlus : PiArg                                  { [$1] }
          | PiArg arrow PiArrPlus                  { $1 : $3 }

Pi : forall VarTypePlus '.' Type                   { foldr (uncurry Pi) $4 $2 }
   | PiArrPlus App                                 { foldr (\(s, t) r -> Pi s t r) $2 $1 }

App : Atom                                         { $1 }
    | App Atom                                     { App $1 $2 }

Atom : var                                         { Var $1 }
     | Kind                                        { $1 }
     | '(' Expr ')'                                { $2 }

Kind : '*'                                         { Kind Star }
     | '[' num ']'                                 { Kind (Box $2) }

{
matchH :: Expr -> [Name] -> Expr -> Expr
matchH _ [] e = e
matchH (Pi v t t') (a:as) e | v == a || v == "_" = (Lam a t e')
  where e' = matchH t' as e
matchH _ _ _ = error "bind type not match"

eLet' :: (Name, Type, Maybe Expr) -> Expr -> Expr
eLet' (s, t, Nothing) b = Lam s t b
eLet' (s, t, Just e) b = Let s t e b

eLets' :: [(Name, Type, Maybe Expr)] -> Expr -> Expr
eLets' stes b = foldr eLet' b stes

addT (s, t) r = Pi s t r
addE (s, t) e = Lam s t e

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}