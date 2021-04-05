module Lib where

import Data.Char

titleCaseToSnakeCase :: String -> String
titleCaseToSnakeCase title = dropWhile (== '_') $ title >>= prefixUnderOnCaps
  where
    prefixUnderOnCaps c = if isUpper c then ['_', toLower c] else [toLower c]
