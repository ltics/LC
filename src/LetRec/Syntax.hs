module LetRec.Syntax where

import LetRec.Context

type Name = String

data BinOp = Add
           | Sub
           | Mul
           | Div
           deriving (Eq, Show)

data Expr = ENum Int
          | EVar Name
          | EBinOp BinOp Expr Expr
          | EIsZero Expr
          | EIf Expr Expr Expr
          | EAbs Name Expr
          | EApp Expr Expr
          | ELet Name Expr Expr
          -- first is fn name, second is param name
          | ELetRec Name Name Expr Expr
          deriving (Eq, Show)

type Closure = (Name, Expr, Context Val)

data Val = VNum Int
         | VBool Bool
         | VClosure Closure
         deriving (Eq, Show)

-- there is no type checker here, so dynamic type for now...

numVal :: Val -> Int
numVal (VNum n) = n
numVal _ = error "Value is not a number"

boolVal :: Val -> Bool
boolVal (VBool b) = b
boolVal _ = error "Value is not a boolean"

closureVal :: Val -> Closure
closureVal (VClosure c) = c
closureVal _ = error "Value is not a closure"