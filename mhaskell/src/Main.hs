module Main where


import           System.Directory               ( getCurrentDirectory )
import           System.Environment
import           Type
import           Parser
import           PatternMatch
import           LambdaLift
import           TypeInference
import           GCompile
import           GEval
import           GPrint

main :: IO ()
main = do
  (command : path : _) <- getArgs
  cDirectory           <- getCurrentDirectory
  content              <- readFile (cDirectory ++ "/" ++ path)
  case command of
    "run"   -> runMachine content
    "check" -> runTypeCheck content

runMachine :: String -> IO ()
runMachine s = case parse s of
  Right p ->
    print $ getOutput . last $ eval $ compile $ liftLambda $ matching p
  Left _ -> error "something went wrong"

runTypeCheck :: String -> IO ()
runTypeCheck s = case parse s of
  Right p -> print $ snd $ inferProgram $ matching p
  Left  _ -> error "something went wrong"
