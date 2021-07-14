import Control.Monad
import Data.List
import System.Random hiding (random)

random :: IO Double
random = randomIO

amplify :: (Int, Int) -> Double -> Int
amplify (min, max) x = floor (x * fromIntegral (max - min + 1)) + min

randRange range = amplify range <$> random

choice items = (items !!) <$> randRange (0, length items - 1)

choices items n = replicateM n (choice items)

sample n randomGenerator = map (\g -> (head g, length g)) . group . sort <$> replicateM n randomGenerator

dice = [1 .. 6]

diceThrow = choice dice

nDiceThrows = choices dice

sumOfNDiceThrows x = sum <$> nDiceThrows x

diceGame = diceThrow >>= sumOfNDiceThrows

-- >>= :: Monad m => m a -> (a -> m b) -> m b

data Move = Rock | Paper | Scissors deriving (Show)

rockPaperScissorsGame = choice [Rock, Paper, Scissors]

main = sample 500000 diceGame >>= print
