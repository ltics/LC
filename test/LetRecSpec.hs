module LetRecSpec where

import LetRec.Syntax
import LetRec.Eval
import LetRec.Parser
import Test.Hspec

spec :: Spec
spec = describe "evaluation test" $ do
        it "should get value of expressions" $ do
          (toplevel . parseExpr) "letrec fact x = if zero? x then 1 else x * fact (x - 1) in fact 5" `shouldBe` VNum 120 --recursion
          (toplevel . parseExpr) "let a = 3 in let p = λ z. a in let f = λ x. p 0 in let a = 5 in f 2" `shouldBe` VNum 3 -- lexical scope
