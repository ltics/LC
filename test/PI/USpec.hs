module PI.USpec where

import PI.U
import Test.Hspec

z = Var "z"
s = Var "s"
m = Var "m"
n = Var "n"

app2 f x y = App (App f x) y
-- church encoding
zero  = Lam "s" $ Lam "z" z
one   = Lam "s" $ Lam "z" $ App s z
two   = Lam "s" $ Lam "z" $ App s $ App s z
three = Lam "s" $ Lam "z" $ App s $ App s $ App s z
plus  = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ app2 m s (app2 n s z)

spec :: Spec
spec = describe "evaluation test" $ do
        it "should get value of expressions" $ do
          betaEq (app2 plus one two) three `shouldBe` True
