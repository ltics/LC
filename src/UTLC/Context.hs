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

pickFreshName :: Name -> Name
pickFreshName x = unsafePerformIO $ do
  ctx <- readIORef globalContext
  let x' = pickFreshName' x ctx
  addName x'
  return x'

name2index :: Name -> IO Index
name2index x = do
  ctx <- readIORef globalContext
  case elemIndex x ctx of
    Just idx -> return idx
    Nothing -> error "Unbound variable"

index2name :: Index -> Name
index2name idx = unsafePerformIO $ do
  ctx <- readIORef globalContext
  if (length ctx) > idx
  then return $ ctx !! idx
  else error "index out of context range"
