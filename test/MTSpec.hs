module MTSpec where

import MT.Syntax
import MT.Eval
import qualified Data.Map as M
import Test.Hspec

spec :: Spec
spec = describe "evaluation test" $ do
        it "should get value of expressions" $ do
          eval0 M.empty (Num 12 `Add` (App (Abs "x" (Var "x")) (Num 4 `Add` Num 2))) `shouldBe` IntVal 18
          runEval1 (eval1 M.empty (Num 12 `Add` (App (Abs "x" (Var "x")) (Num 4 `Add` Num 2)))) `shouldBe` IntVal 18
          runEval2 (eval2 M.empty (Num 1 `Add` (Abs "x" (Var "x")))) `shouldBe` Left "type error in addition"
          runEval2 (eval2 M.empty (Var "x")) `shouldBe` Left "unbound variable: x"
          runEval2 (eval2 M.empty (Num 12 `Add` (App (Abs "x" (Var "x")) (Num 4 `Add` Num 2)))) `shouldBe` Right (IntVal 18)
