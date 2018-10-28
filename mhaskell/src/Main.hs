module Main where

import           Type
import           Parser
import           LambdaLift
import           GCompile
import           GEval
import           GPrint

main :: IO ()
main = putStrLn "Hello, mHaskell!"

runMachine :: String -> IO ()
runMachine s = case parse s of
  Right p -> putStrLn $ showResults $ eval $ compile $ liftLambda p
  Left  _ -> error "Machine.hs#L12"
