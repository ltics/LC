module COC.SSpec where

import COC.S
import Test.Hspec

spec :: Spec
spec = describe "type checker test" $ do
        it "should check type of expressions" $ do
          typeCheck (Lam "s" (Base `Arrow` Base) (Var "s")) `shouldBe` (Base `Arrow` Base) `Arrow` (Base `Arrow` Base)
          typeCheck (Lam "s" (Base `Arrow` Base) (Lam "z" Base (App (Var "s") (Var "z")))) `shouldBe` (Base `Arrow` Base) `Arrow` (Base `Arrow` Base)
