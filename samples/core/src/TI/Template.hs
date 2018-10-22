module TI.Template where

import           Text.Parsec                    ( parse )
import           Parser
import           TI.State
import           TI.Compile
import           TI.Eval
import           TI.Print

runTemplate :: String -> IO ()
runTemplate s = case parse pProgram "core" s of
  Right p -> putStrLn $ showResults $ eval $ compile p
  Left  _ -> error "template.hs#L13"
