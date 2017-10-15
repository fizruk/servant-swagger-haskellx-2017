module Todo.Server where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Todo.API
import Servant

type TodoState = [Todo]

-- | Combined server of a Todo service with Swagger documentation.
server :: TVar TodoState -> Server TodoAPI
server s
    = getAllTodos s
 :<|> createTodo s
 :<|> getTodoById s
 :<|> updateTodoById s

getAllTodos :: TVar TodoState -> Handler [Todo]
getAllTodos s = liftIO $ readTVarIO s

createTodo :: TVar TodoState -> Todo -> Handler TodoId
createTodo s todo = liftIO $ atomically $ do
  todos <- readTVar s
  writeTVar s (todos ++ [todo])
  return (TodoId (length todos))

getTodoById :: TVar TodoState -> TodoId -> Handler Todo
getTodoById s (TodoId i) = do
  todos <- liftIO $ readTVarIO s
  case drop i todos of
    (todo:_) -> return todo
    _ -> throwError err404

updateTodoById :: TVar TodoState -> TodoId -> Todo -> Handler TodoId
updateTodoById s (TodoId i) todo = do
  success <- liftIO $ atomically $ do
    todos <- readTVar s
    let (xs, ys) = splitAt i todos
    case ys of
      (_:zs) -> do
        writeTVar s (xs ++ [todo] ++ zs)
        return True
      _ -> return False
  if success
    then return (TodoId i)
    else throwError err404
