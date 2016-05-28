module UTLC.Eval where

import UTLC.Syntax
import UTLC.Parser
import Control.Monad

isValue :: Term -> Bool
isValue (TmApp _ _) = False
isValue _  = True

needReduction :: Term -> Bool
needReduction (TmVar _ _) = False
needReduction (TmApp _ _) = True
needReduction (TmAbs _ body) = needReduction body

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs _ t12) v2) = return $ termSubstTop v2 t12
eval1 (TmApp t1 t2)
  | isValue t1 = liftM2 TmApp (return t1) (eval1 t2)
  | otherwise  = liftM2 TmApp (eval1 t1) (return t2)
eval1 t@(TmAbs name body) = if needReduction body
                            then liftM (TmAbs name) (eval1 body)
                            else Nothing
eval1 _ = Nothing

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

toplevel :: String -> Term
toplevel = eval . parseExpr
