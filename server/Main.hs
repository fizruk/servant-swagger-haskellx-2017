module Main where

import Network.Wai.Handler.Warp
import Servant

import Todo.API
import Todo.Server

main :: IO ()
main = do
  putStrLn "Running on port 8000"
  run 8000 $ serve (Proxy :: Proxy TodoAPI) server

