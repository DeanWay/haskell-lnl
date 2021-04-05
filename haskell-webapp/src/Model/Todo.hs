{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Todo where

import Control.Lens.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import Lib

data TodoStatus = InProgress | Completed deriving (Show, Eq, Generic)

data Todo = Todo
  { _todoId :: Text,
    _description :: Text,
    _status :: TodoStatus
  }
  deriving (Generic, Show)

data TodoCreateRequest = TodoCreateRequest
  { _description :: Text,
    _status :: TodoStatus
  }
  deriving (Generic, Show)

-- boilerplate
makeFieldsNoPrefix ''Todo
makeFieldsNoPrefix ''TodoCreateRequest

deriveJSON defaultOptions {constructorTagModifier = titleCaseToSnakeCase} ''TodoStatus
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Todo
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''TodoCreateRequest
