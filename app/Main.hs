module Main where

import PI.PI
import PI.Parser
import Control.Monad (foldM)
import Control.Monad.Trans
import System.Console.Haskeline

libFiles :: [String]
libFiles = ["./src/PI/lib/bool.cube",
            "./src/PI/lib/pair.cube",
            "./src/PI/lib/nat.cube",
            "./src/PI/lib/natmisc.cube",
            "./src/PI/lib/list.cube",
            "./src/PI/lib/listmisc.cube",
            "./src/PI/lib/maybe.cube",
            "./src/PI/lib/misc.cube"
           ]

loadFiles :: IO String
loadFiles = foldM (\content file -> do
                    fileContent <- readFile file
                    return $ content ++ fileContent)
                  "" libFiles

readAndCheck :: String -> IO (Maybe (Expr, Type))
readAndCheck line = do
  fileContents <- loadFiles
  let expr = parseExpr $ "let " ++ fileContents ++ " in " ++ line
  case typeCheck expr of
    Left msg -> do putStrLn $ "Type error: " ++ msg; return Nothing
    Right typ -> return $ Just (expr, typ)

evalPrint :: String -> IO ()
evalPrint line = do
  mt <- readAndCheck line
  case mt of
    Nothing -> return ()
    Just (e, t) -> do
      let v = nf e
      putStrLn $ show v ++ " :: " ++ show t
  return ()

loop :: InputT IO ()
loop = do
  minput <- getInputLine "λ> "
  case minput of
    Nothing -> do
      outputStrLn "Goodbye."
      return ()
    Just input -> (liftIO $ evalPrint input) >> loop

main :: IO ()
main = runInputT defaultSettings loop
