{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Data.Text (pack)
import qualified Data.UUID.V4 as UUID
import Route.Todo
import Web.Scotty hiding (status)

runApplication :: Config -> IO ()
runApplication config =
  scotty 3000 $ do
    get "/todo/:todoId" getTodo
    get "/todos" getTodos
    post "/todo" postTodo

main :: IO ()
main = getConfig >>= runApplication
