module UTLCSpec where

import UTLC.Syntax
import UTLC.Eval
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP
import Test.Hspec

runEvalSpecCases :: [(String, String)] -> IO ()
runEvalSpecCases exprExpects = do
    let (vals, expects) = foldl (\(vals, expects) (expr, expect) -> (vals ++ [toplevel expr], expects ++ [expect]))
                                ([], []) exprExpects
    (map showTerm vals) `shouldBe` map PP.text expects

spec :: Spec
spec = describe "evaluation test" $ do
        it "should get value of ADT and pattern match expressions part1" $ do
          runEvalSpecCases [("(λ n. λ s. λ z. s (n s z)) (λ s. λ z. z)", "(λ s. (λ z. (s z)))"), -- succ 0 = 1
                            ("(λ x. x) (λ x. x)", "(λ x. x)")]
