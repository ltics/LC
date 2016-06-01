module STLCSpec where

import STLC.Syntax
import STLC.Eval
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP
import Prelude hiding (succ)
import Test.Hspec

runEvalSpecCases :: [(String, Term)] -> IO ()
runEvalSpecCases exprExpects = do
    let (vals, expects) = foldl (\(vals, expects) (expr, expect) -> (vals ++ [toplevel expr], expects ++ [expect]))
                                ([], []) exprExpects
    vals `shouldBe` expects

succ = "(λ n: (Int → Int) → Int → Int. λ s: Int → Int. λ z: Int. s (n s z))"
zero = "(λ s: Int → Int. λ z: Int. z)"
one = "(λ s: Int → Int. (λ z: Int. (s z)))"

spec :: Spec
spec = describe "evaluation test" $ do
        it "should get value of utlc terms" $ do
          showTerm (toplevel (succ ++ " " ++ zero)) `shouldBe` PP.text one
          runEvalSpecCases [("(λ x: Int. succ x) 0", TmSucc TmZero),
                            ("(λ x: Int. succ (succ x)) (succ 0)", TmSucc $ TmSucc $ TmSucc TmZero),
                            ("(λ x: Int → Bool. if (x 0) then (succ 0) else (pred 0)) (λ x:Int. zero? x)", TmSucc TmZero),
                            ("(λ x: Int. if zero? x then succ x else x) (succ 0)", TmSucc TmZero),
                            ("let a = λx: Int. succ x in (a 0)", TmSucc TmZero),
                            -- desuger
                            ("(λa: Int → Int. a 0) (λx: Int. succ x)", TmSucc TmZero),
                            ("letrec a: Int → Int = (λ x: Int. if zero? x then succ 0 else a (pred x)) in a (succ (succ (succ 0)))", TmSucc TmZero),
                            -- desuger
                            ("let a = fix (λa: Int → Int. λ x: Int. if zero? x then succ 0 else a (pred x)) in a (succ (succ (succ 0)))", TmSucc TmZero),
                            -- desuger
                            ("(λa: Int → Int. a (succ (succ (succ 0)))) (fix (λa: Int → Int. λ x: Int. if zero? x then succ 0 else a (pred x)))", TmSucc TmZero)]
