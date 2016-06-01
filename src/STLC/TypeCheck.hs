module STLC.TypeCheck where

import STLC.Syntax
import STLC.Type
import STLC.Context

check :: Context -> Term -> Type
check _ TmTrue = TBool
check _ TmFalse = Right TBool
check _ TmZero = Right TInt
check ctx (TmSucc term)
  | check ctx term == TInt = TInt
  | otherwise = error "argument of succ is not a number"
check ctx (TmPred term)
  | check ctx term == TInt = TInt
  | otherwise = error "argument of pred is not a number"
check ctx (TmIsZero term)
  | check ctx term == TInt = TBool
  | otherwise = error "argument of iszero is not a number"
check ctx (TmVar x _) = case getType x ctx of
                          Just (VarBind ty) -> ty
                          _ -> error "can not find type of var"
check ctx (TmAbs x ty body) = let ctx' = (x, VarBind ty) : ctx
                              in ty :~> check ctx' body
check ctx (TmApp fn arg) = case check ctx fn of
                             argT :~> rtnT -> if argT == check ctx arg
                                             then rtnT
                                             else error "parameter type mismatch"
                             _ -> error "arrow type expected"
check ctx (TmIf cond consequent alternative)
  | check ctx cond == TBool = if check ctx consequent == check ctx alternative
                             then check ctx consequent
                             else error "arms of conditional have different types"
  | otherwise = error "guard of conditional not a boolean"
check ctx (TmLet x def body) = check ctx' body
  where defT = check ctx def
        ctx' = (x, VarBind defT) : ctx
check ctx (TmFix term) = case check ctx term of
                           argT :~> rtnT | argT == rtnT = rtnT
                                         | error "result of body not compatible with domain"
                           _ -> error "arrow type expected"
