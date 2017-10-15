module Main where

import Control.Concurrent.STM (newTVarIO)
import Network.Wai.Handler.Warp
import Servant

import Todo.API
import Todo.Server

main :: IO ()
main = do
  todos <- newTVarIO []
  putStrLn "Running on port 8000"
  run 8000 $ serve (Proxy :: Proxy API) (serverWithSwaggerUI todos)
