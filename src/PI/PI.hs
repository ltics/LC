module PI.PI where

import Data.List (union, (\\))
import Control.Monad.Except

type Name = String

data Expr = Var Name
          | App Expr Expr
          | Lam Name Type Expr
          | Pi Name Type Type
          | Let Name Type Expr Expr
          | Kind Kind
          deriving (Eq, Show)

type Type = Expr

data Kind = Star
          | Box Int
          deriving (Eq, Show)

-- desuger let to lambda
expandLet :: Name -> Type -> Expr -> Expr -> Expr
expandLet i t e b = App (Lam i t b) e

freeVars :: Expr -> [Name]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i t e) = freeVars t `union` (freeVars e \\ [i])
freeVars (Pi i k t) = freeVars k `union` (freeVars t \\ [i])
freeVars (Let i t e b) = freeVars (expandLet i t e b)
freeVars (Kind _) = []

subst :: Name -> Expr -> Expr -> Expr
subst v x = sub
  where sub e@(Var i) = if i == v then x else e
        sub (App f a) = App (sub f) (sub a)
        sub (Lam i t e) = abstr Lam i t e
        sub (Pi i t e) = abstr Pi i t e
        sub (Let i t e b) =
          let App (Lam i' t' b') e' = sub (expandLet i t e b)
          in Let i' t' e' b'
        sub (Kind k) = Kind k
        fvx = freeVars x
        cloneName e i = loop i
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = fvx ++ freeVars e
        abstr con i t e =
            if v == i then con i (sub t) e
            else if i `elem` fvx
            then let i' = cloneName e i
                     e' = substVar i i' e
                 in  con i' (sub t) (sub e')
            else con i (sub t) (sub e)

whnf :: Expr -> Expr
whnf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s _ e) (a:as) = spine (subst s a e) as
        spine (Let i t e b) as = spine (expandLet i t e b) as
        spine f as = foldl App f as

nf :: Expr -> Expr
nf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s t e) [] = Lam s (nf t) (nf e)
        spine (Lam s _ e) (a:as) = spine (subst s a e) as
        spine (Pi s k t) as = app (Pi s (nf k) (nf t)) as
        spine (Let i t e b) as = spine (expandLet i t e b) as
        spine f as = app f as
        app f as = foldl App f (map nf as)

substVar :: Name -> Name -> Expr -> Expr
substVar s s' e = subst s (Var s') e

alphaEq :: Expr -> Expr -> Bool
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s t e) (Lam s' t' e') = alphaEq t t' && alphaEq e (substVar s' s e')
alphaEq (Pi s k t) (Pi s' k' t')  = alphaEq k k' && alphaEq t (substVar s' s t')
alphaEq (Let s t e b) (Let s' t' e' b') = alphaEq t t' && alphaEq e e' && alphaEq b (substVar s' s b')
alphaEq (Var s) (Var s') = s == s'
alphaEq (Kind k) (Kind k') = k == k'
alphaEq _ _ = False

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

type ErrorMsg = String

data Env = Env [(Name, Type)] deriving (Show)

inital :: Env
inital = Env []

extend :: Name -> Type -> Env -> Env
extend s t (Env r) = Env ((s, t) : r)

findVar :: Env -> Name -> TC Type
findVar (Env r) s = case lookup s r of
                      Just t -> return t
                      Nothing -> throwError ("Cannot find variable " ++ s)

type TC a = Either ErrorMsg a

tCheck :: Env -> Expr -> TC Type
tCheck r (Var s) = findVar r s
tCheck r (Let s t a e) = do
  tCheck r t
  ta <- tCheck r a
  when (not (betaEq ta t)) $ throwError $ "Bad let def\n" ++ show (ta, t)
  te <- tCheck r (subst s a e)
  tCheck r (Pi s t te)
  return te
tCheck r (App f a) = do
  tf <- tCheckRed r f
  case tf of
    -- pi type is just an extended arrow type
    Pi x at rt -> do
      ta <- tCheck r a
      when (not (betaEq ta at)) $ throwError $ "Bad function argument type:\n" ++
                                             "Function: " ++ show (nf f) ++ "\n" ++
                                             "argument: " ++ show (nf a) ++ "\n" ++
                                             "expected type: " ++ show at ++ "\n" ++
                                             "     got type: " ++ show ta
      return $ subst x a rt
    _ -> throwError $ "Non-function in application: " ++ show tf
tCheck r (Lam s t e) = do
  tCheck r t
  let r' = extend s t r
  te <- tCheck r' e
  -- pi type is just an extended arrow type remember?
  let lt = Pi s t te
  tCheck r lt
  return lt
tCheck r (Pi x a b) = do
  s <- tCheckRed r a
  let r' = extend x a r
  t <- tCheckRed r' b
  when ((s, t) `notElem` allowedKinds) $ throwError $ "Bad abstraction: " ++ show (Pi x a b)
  return t
tCheck _ (Kind Star) = return $ Kind $ Box 0
-- type of Box.{i} should be Box.{i+1}
tCheck _ (Kind (Box i)) =  return $ Kind $ Box (i + 1)

allowedKinds :: [(Type, Type)]
allowedKinds = [(Kind Star, Kind Star), -- values depend on values, yield the λ→
                (Kind (Box 0), Kind Star), -- values depend on types
                (Kind (Box 0), Kind (Box 0)), -- types depend on types
                -- three combinations above yield Fω
                (Kind Star, Kind (Box 0)) -- types depend on values, yield the λπ
               ]

tCheckRed :: Env -> Expr -> TC Type
tCheckRed r e = do
  t <- tCheck r e
  return $ whnf t

typeCheck :: Expr -> Either ErrorMsg Type
typeCheck e = fmap nf $ tCheck inital e
