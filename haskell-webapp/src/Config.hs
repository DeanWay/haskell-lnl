module Config where

import System.Environment hiding (getEnvironment)

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

getAppEnv :: IO Environment
getAppEnv = maybe Development read <$> lookupEnv "TODO_APP_ENV"

data Config = Config
  { environment :: Environment,
    pool :: String
  }

getConfig :: IO Config
getConfig = do
  env <- getAppEnv
  return
    Config
      { environment = env,
        pool = "lol"
      }
