module UTLC.Syntax where

type Name = String
type Field = String
type Context = [(Name, Binding)]

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmVar Int Int -- the last value store the lambda abs count in global context
          | TmAbs Name Term
          | TmApp Term Term
          | TmRecord [(Field, Term)]
          | TmProj Term Field
          | TmString String
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmLet Name Term Term
          deriving (Show)

data Binding = Binding Term deriving (Show)

data Expr = Eval Term
          | Bind Name Binding
          deriving (Show)

-- Shifting

walkTmMap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
walkTmMap onvar c t = case t of
                        TmTrue -> t
                        TmFalse -> t
                        TmIf cond con alt -> TmIf (walkTmMap onvar c cond) (walkTmMap onvar c con) (walkTmMap onvar c alt)
                        TmVar k n -> onvar c k n
                        TmAbs name body -> TmAbs name (walkTmMap onvar (c + 1) body)
                        TmApp fn arg -> TmApp (walkTmMap onvar c fn) (walkTmMap onvar c arg)
                        TmRecord pairs -> TmRecord (map (\(k, v) -> (k, (walkTmMap onvar c v))) pairs)
                        TmProj record field -> TmProj (walkTmMap onvar c record) field
                        TmString _ -> t
                        TmZero -> t
                        TmSucc n -> TmSucc (walkTmMap onvar c n)
                        TmPred n -> TmPred (walkTmMap onvar c n)
                        TmIsZero n -> TmIsZero (walkTmMap onvar c n)
                        TmLet name def body -> TmLet name (walkTmMap onvar c def) (walkTmMap onvar (c + 1) body)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t = walkTmMap (\c k n -> if k >= c then TmVar (k + d) (n + d) else TmVar k (n + d)) c t

termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

bindingShift :: Int -> Binding -> Binding
bindingShift d (Binding t) = Binding (termShift d t)

-- Substitution

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walkTmMap (\c k n -> if k == j + c then termShift c s else TmVar k n) 0 t

-- beta reduction
termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
