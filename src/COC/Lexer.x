{
module COC.Lexer where
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
       "--".*                      ; --comments
       "∀"                         { \_ -> FORALL }
       "λ"                         { \_ -> LAMBDA }
       "→"                         { \_ -> ARROW }
       "("                         { \_ -> LPAREN }
       ")"                         { \_ -> RPAREN }
       "["                         { \_ -> LBRACKET }
       "]"                         { \_ -> RBRACKET }
       "."                         { \_ -> DOT }
       "::"                        { \_ -> DOUBLECOLON }
       ";"                         { \_ -> SEMICOLON }
       "*"                         { \_ -> STAR }
       "="                         { \_ -> IFF }
       "_"                         { \_ -> WILDCARD }
       let                         { \_ -> LET }
       in                          { \_ -> IN }
       $chars+                     { \s -> VAR s }
       $digit+                     { \s -> NUM (read s) }

{
data Token = FORALL
           | LAMBDA
           | ARROW
           | LPAREN
           | RPAREN
           | LBRACKET
           | RBRACKET
           | DOT
           | DOUBLECOLON
           | SEMICOLON
           | STAR
           | IFF
           | WILDCARD
           | LET
           | IN
           | VAR String
           | NUM Int
           deriving(Eq, Show)

scanTokens = alexScanTokens
}