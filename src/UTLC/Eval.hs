module UTLC.Eval where

import UTLC.Syntax
import Control.Monad

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs _ t12) v2@(TmAbs _ _)) = return $ termSubstTop v2 t12
eval1 (TmApp v1@(TmAbs _ _) t2) = liftM2 TmApp (return v1) (eval1 t2)
eval1 (TmApp t1 t2) = liftM2 TmApp (eval1 t1) (return t2)
eval1 _ = Nothing

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

