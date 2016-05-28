module UTLCSpec where

import UTLC.Syntax
import UTLC.Eval
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP
import Prelude hiding (succ, not, or, and)
import Test.Hspec

runEvalSpecCases :: [(String, String)] -> IO ()
runEvalSpecCases exprExpects = do
    let (vals, expects) = foldl (\(vals, expects) (expr, expect) -> (vals ++ [toplevel expr], expects ++ [expect]))
                                ([], []) exprExpects
    (map showTerm vals) `shouldBe` map PP.text expects

succ   = "(λ n. λ s. λ z. s (n s z))"
mult   = "(λ n. λ s. λ z. n (s z))"
zero   = "(λ s. λ z. z)"
one    = "(λ s. (λ z. (s z)))"
two    = "(λ s. (λ z. (s (s z))))"
three  = "(λ s. (λ z. (s (s (s z)))))"
five   = "(λ s. (λ z. (s (s (s (s (s z)))))))"
six    = "(λ z. (λ z'. (z (z (z (z (z (z z'))))))))"
true   = "(λ s. (λ z. s))"
false  = "(λ s. (λ z. z))"
and    = "(λ s. λ z. s z " ++ false ++ ")"
or     = "(λ s. λ z. s " ++ true ++ " z)"
not    = "(λ s. s " ++ false ++ " " ++ true ++ ")"
iszero = "(λ s. s " ++ false ++ " " ++ not ++ " " ++ false ++ ")"

spec :: Spec
spec = describe "evaluation test" $ do
        it "should get value of ADT and pattern match expressions part1" $ do
          runEvalSpecCases [(succ ++ " " ++ zero, one), -- succ 0 = 1
                            ("(λ x. x) (λ x. x)", "(λ x. x)"), -- id id = id
                            ("(λ x. x x) (λ x. x)", "(λ x. x)"),
                            (two ++ " " ++ succ ++ " " ++ three, five), -- 2 + 3 = 5
                            (mult ++ " " ++ two ++ " " ++ three, six), -- 2 * 3 = 6
                            (and ++ " " ++ false ++ " " ++ true , false),
                            (and ++ " " ++ true ++ " " ++ true, true),
                            (or ++ " " ++ true ++ " " ++ false, true),
                            (or ++ " " ++ false ++ " " ++ false, false),
                            (not ++ " " ++ false, true),
                            (not ++ " " ++ true, false),
                            (iszero ++ " " ++ zero, true),
                            (iszero ++ " " ++ three, false)]
