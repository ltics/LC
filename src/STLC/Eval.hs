module STLC.Eval where

import STLC.Syntax
import STLC.Parser
import Control.Monad

needReduction :: Term -> Bool
needReduction (TmVar _ _) = False
needReduction (TmApp _ _) = True
needReduction (TmAbs _ _ body) = needReduction body

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs _ _ t12) v2) = return $ termSubstTop v2 t12
eval1 (TmApp t1@(TmApp _ _) t2) = liftM2 TmApp (eval1 t1) (return t2)
eval1 (TmApp t1 t2@(TmApp _ _)) = liftM2 TmApp (return t1) (eval1 t2)
eval1 (TmAbs name t body) = if needReduction body
                            then liftM (TmAbs name t) (eval1 body)
                            else Nothing
eval1 _ = Nothing

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

toplevel :: String -> Term
toplevel = eval . parseExpr
