module Machine where

import           Text.Parsec                    ( parse )
import           Parser
import           G.Print
import           G.Eval
import           G.Compile

runMachine :: String -> IO ()
runMachine s = case parse pProgram "core" s of
  Right p -> putStrLn $ showResults $ eval $ compile p
  Left  _ -> error "Machine.hs#L12"
