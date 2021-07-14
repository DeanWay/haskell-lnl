import Control.Concurrent
import Control.Monad

data TrafficLightColor = RedLight | YellowLight | GreenLight deriving (Eq, Show)

data TrafficLightStep = TrafficLightStep
  { lightColor :: TrafficLightColor,
    durationMiliseconds :: Int
  }

type TrafficLightTimerConfig = [TrafficLightStep]

data Name = RobotName Int | HumanName String

names = [RobotName 12345, HumanName "Steven"]

trafficLightIterpretedMeaning color = case color of
  RedLight -> "Stop"
  GreenLight -> "Go"
  YellowLight -> "Go faster"

runTrafficLightStep TrafficLightStep {lightColor = color, durationMiliseconds = delay} = do
  putStrLn $ show color ++ " - " ++ trafficLightIterpretedMeaning color
  threadDelay $ delay * 1000 --threadDelay takes microseconds

runTrafficLight :: TrafficLightTimerConfig -> IO ()
runTrafficLight timerConfig = forM_ timerConfig runTrafficLightStep

main = do
  let config =
        [ TrafficLightStep {lightColor = GreenLight, durationMiliseconds = 3000},
          TrafficLightStep {lightColor = YellowLight, durationMiliseconds = 1000},
          TrafficLightStep {lightColor = RedLight, durationMiliseconds = 3000}
        ]
  runTrafficLight $ cycle config
