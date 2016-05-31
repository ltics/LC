module LetRec.Eval where

import LetRec.Syntax
import LetRec.Context

evalClosure :: Closure -> Val -> Val
evalClosure (x, e, ctx) v = eval e (insertVal x v ctx)

-- big step

eval :: Expr -> Context Val -> Val
eval (ENum n) _ = VNum n
eval (EVar x) ctx = findVal ctx x
eval (EBinOp op e1 e2) ctx =
  let n1 = numVal (eval e1 ctx)
      n2 = numVal (eval e2 ctx)
  in case op of
       Add -> VNum (n1 + n2)
       Sub -> VNum (n1 - n2)
       Mul -> VNum (n1 * n2)
       Div -> VNum (n1 `div` n2)
eval (EIsZero e) ctx =
  let n = numVal (eval e ctx)
  in VBool (n == 0)
eval (EIf e1 e2 e3) ctx =
  if boolVal (eval e1 ctx)
    then eval e2 ctx
    else eval e3 ctx
eval (ELet x e1 e2) ctx =
  let v1 = eval e1 ctx
  in eval e2 (insertVal x v1 ctx)
eval (ELetRec fn x e1 e2) ctx =
  -- little trick here
  -- use lazy semantics instead of manipulate global mutable context to implement recursion
  let ctx' = insertVal fn closure ctx
      closure = VClosure (x, e1, ctx')
  in eval e2 ctx'
eval (EAbs x e) ctx = VClosure (x, e, ctx)
eval (EApp e1 e2) ctx =
  let proc = closureVal (eval e1 ctx)
      arg = eval e2 ctx
  in evalClosure proc arg

toplevel :: Expr -> Val
toplevel e = eval e initEnv