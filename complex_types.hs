{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

data Name
  = RobotName Int
  | HumanName String
  | SpaceshipName {modelName :: String, serialNumber :: String}

names =
  [ RobotName 43110,
    HumanName "Steven",
    SpaceshipName {modelName = "Star Destoryer", serialNumber = "A2D4"}
  ]

class Named a where
  name :: a -> String

newtype Robot = Robot Int

instance Named Robot where
  name (Robot num) = show num

newtype Human = Human String

instance Named Human where
  name (Human s) = s

data SpaceShip = SpaceShip
  { modelName :: String,
    serialNumber :: String,
    maxSpeed :: Integer
  }

instance Named SpaceShip where
  name SpaceShip {modelName, serialNumber} = modelName ++ " - " ++ serialNumber
