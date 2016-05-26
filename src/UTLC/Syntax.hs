module UTLC.Syntax where

type Name = String
type Field = String

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmVar Int Int -- the last value store the lambda abs count in global context
          | TmAbs Name Term
          | TmApp Term Term
          | TmRecord [(Field, Term)]
          | TmProj Term Field
          | TmFloat Float
          | TmString String
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmLet Name Term Term

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
                        TmFloat _ -> t
                        TmString _ -> t
                        TmZero -> t
                        TmSucc n -> TmSucc (walkTmMap onvar c n)
                        TmPred n -> TmPred (walkTmMap onvar c n)
                        TmIsZero n -> TmIsZero (walkTmMap onvar c n)
                        TmLet name def body -> TmLet name (walkTmMap onvar c def) (walkTmMap onvar (c + 1) body)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t = walkTmMap (\c k n -> if k >= c then (TmVar (k + d) (n + d)) else (TmVar k (n + d))) c t

termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

-- Substitution
