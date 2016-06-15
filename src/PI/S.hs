module PI.S where

import Control.Monad.Except

type Name = String

-- simple types
data Type = Base
          | Arrow Type Type
          deriving (Eq, Read, Show)

data Expr = Var Name
          | App Expr Expr
          | Lam Name Type Expr
          deriving (Eq, Read, Show)

newtype Env = Env [(Name, Type)] deriving (Show)

inital :: Env
inital = Env []

extend :: Name -> Type -> Env -> Env
extend s t (Env r) = Env ((s, t) : r)

type ErrorMsg = String

type TC a = Either ErrorMsg a

findVar :: Env -> Name -> TC Type
findVar (Env r) s = case lookup s r of
                      Just t -> return t
                      Nothing -> throwError $ "Cannot find variable " ++ s

tCheck :: Env -> Expr -> TC Type
tCheck r (Var s) = findVar r s
tCheck r (App f a) = do
  tf <- tCheck r f
  case tf of
    Arrow at rt -> do
      ta <- tCheck r a
      when (ta /= at) $ throwError "Bad function argument type"
      return rt
    _ -> throwError "Non-function in application"
tCheck r (Lam s t e) = do
  let r' = extend s t r
  te <- tCheck r' e
  return $ Arrow t te

typeCheck :: Expr -> Type
typeCheck e = case tCheck inital e of
                Left msg -> error ("Type error:\n" ++ msg)
                Right t -> t
