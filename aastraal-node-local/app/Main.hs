module Main where

-- import Text.Printf (printf)

import qualified BasicConcurrentServer as Server
import qualified DecisionEngine
-- import qualified Controller

main :: IO ()
main = do
  putStrLn "Starting Aastraal Local Node"
  Server.main DecisionEngine.process
