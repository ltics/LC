module LetRec.Context where

import Data.Maybe (fromJust)

-- generic context
type Context a = [(String, a)]

initEnv :: Context a
initEnv = []

findVal :: Context a -> String -> a
findVal e x = fromJust (lookup x e)

insertVal :: String -> a -> Context a -> Context a
insertVal x t = (:) (x, t)
