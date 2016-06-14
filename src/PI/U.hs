module PI.U where

import Data.List

type Name = String

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          deriving (Eq, Read, Show)

freeVars :: Expr -> [Name]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i e) = freeVars e \\ [i]

subst :: Name -> Expr -> Expr -> Expr
subst v x b = sub b
  where sub e@(Var i) = if i == v then x else e
        sub (App f a) = App (sub f) (sub a)
        sub l@(Lam i e) =
            if v == i then l
            else if i `elem` fvx
            then let i' = cloneName e i
                     e' = substVar i i' e
                 in Lam i' (sub e')
            else Lam i (sub e)
        fvx = freeVars x
        cloneName e i = loop i
           -- pick a totally free variable name
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = fvx ++ freeVars e

substVar :: Name -> Name -> Expr -> Expr
substVar s s' e = subst s (Var s') e

-- weak head normal form
whnf :: Expr -> Expr
whnf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s e) (a:as) = spine (subst s a e) as
        spine f as = foldl App f as

-- alpha conversions just do not care about binder name in lambda abstraction terms
alphaEq :: Expr -> Expr -> Bool
alphaEq (Var v) (Var v') = v == v'
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s e) (Lam s' e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False

-- normal form
nf :: Expr -> Expr
nf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s e) [] = Lam s (nf e)
        spine (Lam s e) (a:as) = spine (subst s a e) as
        spine f as = app f as
        app f as = foldl App f (map nf as)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)
