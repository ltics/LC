module UTLC.Syntax where

type Name = String
type Field = String

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmVar Int Int
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

