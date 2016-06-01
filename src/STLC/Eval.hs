module STLC.Eval where

import STLC.Syntax
import STLC.Parser
import STLC.TypeCheck
import Control.Monad

isNumericVal :: Term -> Bool
isNumericVal t = case t of
                   TmZero -> True
                   TmSucc t -> isNumericVal t
                   _ -> False

-- is val means there is no need with further reduction especially like (succ (succ (succ 0)))
isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal TmZero = True
isVal (TmAbs _ _ _) = True
isVal t = if isNumericVal t
          then True
          else False

isAbs :: Term -> Bool
isAbs (TmAbs _ _ _) = True
isAbs _ = False

needReduction :: Term -> Bool
needReduction (TmVar _ _) = False
needReduction (TmApp _ _) = True
needReduction (TmAbs _ _ body) = needReduction body
needReduction t = if isVal t && (not . isAbs) t
                  then False
                  else True

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs _ _ t12) v2) = return $ termSubstTop v2 t12
eval1 (TmApp t1@(TmApp _ _) t2) = liftM2 TmApp (eval1 t1) (return t2)
eval1 (TmApp t1 t2@(TmApp _ _)) = liftM2 TmApp (return t1) (eval1 t2)
eval1 (TmAbs name t body) = if needReduction body
                            then liftM (TmAbs name t) (eval1 body)
                            else Nothing
eval1 (TmIf TmTrue consequent _ ) = return consequent
eval1 (TmIf TmFalse _ alternative) = return alternative
eval1 (TmIf cond consequent alternative) = liftM (\cond' -> TmIf cond' consequent alternative) (eval1 cond)
eval1 (TmSucc t) = liftM TmSucc (eval1 t)
eval1 (TmPred TmZero) = return TmZero
eval1 (TmPred (TmSucc t))
  | isNumericVal t = return t
eval1 (TmPred t) = liftM TmPred (eval1 t)
eval1 (TmIsZero TmZero) = return TmTrue
eval1 (TmIsZero (TmSucc t))
  | isNumericVal t = return TmFalse
eval1 (TmIsZero t) = liftM TmIsZero (eval1 t)
eval1 (TmLet x def body)
  | isVal def = return $ termSubstTop def body
eval1 (TmLet x def body) = liftM (\def' -> TmLet x def' body) (eval1 def)
eval1 t@(TmFix t')
  | isVal t' = case t' of
                 TmAbs _ _ body -> return $ termSubstTop t body
                 _ -> Nothing
eval1 (TmFix t) = liftM TmFix (eval1 t)
eval1 _ = Nothing

eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

toplevel :: String -> Term
toplevel term = let expr = parseExpr term
                in check [] expr `seq` eval expr
