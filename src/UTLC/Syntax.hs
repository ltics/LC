module UTLC.Syntax where

import UTLC.Context
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.PrettyPrint as PP

data Term = TmVar Int Int -- the last value store the lambda abs count in global context
          | TmAbs Name Term
          | TmApp Term Term
          deriving (Show)

showTerm' :: Term -> Context -> String
showTerm' t ctx = case t of
                    TmVar n _ -> index2name n ctx
                    TmAbs x body -> let (x', ctx') = pickFreshName x ctx
                                   in "(λ " ++ x' ++ "." ++ showTerm' body ctx' ++ ")"
                    TmApp fn arg -> "(" ++ showTerm' fn ctx ++ " " ++ showTerm' arg ctx ++ ")"


showTerm :: Term -> PP.Doc
showTerm t = PP.text $ showTerm' t []

-- Shifting

walkTmMap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
walkTmMap onvar c t = case t of
                        TmVar k n -> onvar c k n
                        TmAbs name body -> TmAbs name (walkTmMap onvar (c + 1) body)
                        TmApp fn arg -> TmApp (walkTmMap onvar c fn) (walkTmMap onvar c arg)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t = walkTmMap (\c k n -> if k >= c then TmVar (k + d) (n + d) else TmVar k (n + d)) c t

termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

-- Substitution

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walkTmMap (\c k n -> if k == j + c then termShift c s else TmVar k n) 0 t

-- beta reduction
termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
