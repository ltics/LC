module MT.Eval where

import MT.Syntax
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as M

eval0 :: Env -> Exp -> Value
eval0 env (Num i) = IntVal i
eval0 env (Var n) = fromJust (M.lookup n env)
eval0 env (Add e1 e2) = let IntVal i1 = eval0 env e1
                            IntVal i2 = eval0 env e2
                        in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                             FunVal env' n body -> eval0 (M.insert n val2 env') body

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Num i) = return $ IntVal i
eval1 env (Var n) = maybe (fail $ "unbound variable: " ++ n) return $ M.lookup n env
eval1 env (Add e1 e2) = do IntVal i1 <- eval1 env e1
                           IntVal i2 <- eval1 env e2
                           return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                             FunVal env' n body -> eval1 (M.insert n val2 env') body

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Num i) = return $ IntVal i
eval2 env (Var n) = case M.lookup n env of
                      Nothing -> throwError ("unbound variable: " ++ n)
                      Just val -> return val
eval2 env (Add e1 e2) = do e1' <- eval2 env e1
                           e2' <- eval2 env e2
                           case (e1', e2') of
                             (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                             _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do val1 <- eval2  env e1
                           val2 <- eval2  env e2
                           case val1 of
                             FunVal env' n body -> eval2 (M.insert n val2 env') body
                             _ -> throwError "type error in application"
