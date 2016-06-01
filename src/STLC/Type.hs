module STLC.Type where

data Type = TUnit
          | TInt
          | TBool
          | Type :~> Type
          deriving (Eq)

instance Show Type where
  show TUnit = "unit"
  show TInt = "Int"
  show TBool = "Bool"
  show (t1 :~> t2) = show t1 ++ " → " ++ show t2
