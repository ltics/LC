module MT.Syntax where

import qualified Data.Map as M

type Name = String

data Exp = Num Int
         | Var Name
         | Add Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show, Eq)

data Value = IntVal Int
           | FunVal Env Name Exp
           deriving (Show, Eq)

type Env = M.Map Name Value
