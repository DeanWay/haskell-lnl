{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Route.Todo where

import Control.Lens
import Control.Monad.IO.Class
import Data.Text (pack)
import qualified Data.UUID.V4 as UUID
import Model.Todo
import Web.Scotty hiding (status)

getTodo :: ActionM ()
getTodo = do
  todoId <- param "todoId"
  json $ Todo {_todoId = todoId, _description = "get things done", _status = InProgress}

getTodos :: ActionM ()
getTodos = do
  json [Todo {_todoId = pack $ show x, _description = "get things done", _status = InProgress} | x <- [1 .. 10]]

postTodo :: ActionM ()
postTodo = do
  requestData <- jsonData
  todo <- liftIO $ saveTodo requestData
  json todo

saveTodo :: TodoCreateRequest -> IO Todo
saveTodo request = do
  uuid <- UUID.nextRandom
  return
    Todo
      { _todoId = pack $ show uuid,
        _description = request ^. description,
        _status = request ^. status
      }
