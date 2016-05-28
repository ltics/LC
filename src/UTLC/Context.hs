module UTLC.Context where

import Data.List
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

type Name = String
type Index = Int
type Context = [Name]

createState :: a -> IORef a
createState = unsafePerformIO . newIORef

globalContext :: IORef Context
globalContext = createState []

addName :: Name -> IO ()
addName x = modifyIORef globalContext (x:)

pickFreshName' :: Name -> Context -> Name
pickFreshName' x [] = x
pickFreshName' x ctx@(h:t)
  | x == h = pickFreshName' (x ++ "'") ctx
  | otherwise = pickFreshName' x t

pickFreshName :: Name -> Context -> (Name, Context)
pickFreshName x ctx = let x' = pickFreshName' x ctx
                      in (x', x' : ctx)

name2index :: Name -> IO Index
name2index x = do
  ctx <- readIORef globalContext
  case elemIndex x ctx of
    Just idx -> return idx
    Nothing -> error "Unbound variable"

index2name :: Index -> Context -> Name
index2name idx ctx
  | length ctx > idx = ctx !! idx
  | otherwise = error $ ("Requested index " ++ show idx ++ " of Context of length " ++ show (length ctx))
