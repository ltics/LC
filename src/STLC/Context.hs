module STLC.Context where

import STLC.Type
import Data.List
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

type Name = String
type Index = Int
type Context = [(Name, Bind)]

data Bind = NameBind
          | VarBind Type
  deriving (Eq, Show)

createState :: a -> IORef a
createState = unsafePerformIO . newIORef

globalContext :: IORef Context
globalContext = createState []

addName :: Name -> Type -> IO ()
addName x t = modifyIORef globalContext ((x, VarBind t):)

pickFreshName' :: Name -> Context -> Name
pickFreshName' x [] = x
pickFreshName' x ctx@(h:t)
  | x == fst h = pickFreshName' (x ++ "'") ctx
  | otherwise = pickFreshName' x t

pickFreshName :: Name -> Context -> (Name, Context)
pickFreshName x ctx = let x' = pickFreshName' x ctx
                      in (x', (x', NameBind) : ctx)

name2index :: Name -> IO Index
name2index x = do
  ctx <- readIORef globalContext
  case findIndex ((== x) . fst) ctx of
    Just idx -> return idx
    Nothing -> error "Unbound variable"

index2name :: Index -> Context -> Name
index2name idx ctx
  | length ctx > idx = fst $ ctx !! idx
  | otherwise = error $ ("Requested index " ++ show idx ++ " of Context of length " ++ show (length ctx))

index2type :: Index -> Context -> Bind
index2type idx ctx
  | length ctx > idx = snd $ ctx !! idx
  | otherwise = error $ ("Requested index " ++ show idx ++ " of Context of length " ++ show (length ctx))
