import Data.Foldable
import Data.Function ((&))
import Data.Graph
import Data.List
import qualified Data.Map as Map
import Debug.Trace

data Query = Query
  { numCities :: Int,
    costOfLib :: Int,
    costOfRoad :: Int,
    possibleRoads :: [(Int, Int)]
  }
  deriving (Show)

connections :: [(Int, Int)] -> [(Int, [Int])]
connections = Map.toList . Map.fromListWith (++) . map (\(x, y) -> (x, [y]))

minCostForUniversalLibAccess :: Query -> Int
minCostForUniversalLibAccess q =
  if costOfLib q < costOfRoad q
    then costOfLib q * numCities q
    else costOfAllRoads + costOfRemainingCities
  where
    costOfAllRoads = sum $ map ((* costOfRoad q) . subtract 1) cityGroupLengths
    costOfRemainingCities = (numCities q - sum cityGroupLengths) * costOfLib q
    graph = buildG (1, numCities q) ([(x, x) | x <- [1 .. numCities q]] ++ map (\(x, y) -> (y, x)) (possibleRoads q) ++ possibleRoads q)
    cityGroupLengths = map length connectedCities
    connectedCities = map (nub . concat . toList) $ bcc graph

parseQueries :: [[Int]] -> [Query]
parseQueries = reverse . fst . parseQueries' []

parseQueries' :: [Query] -> [[Int]] -> ([Query], [[Int]])
parseQueries' queries [] = (queries, [])
parseQueries' queries (infoLine : rest) = parseQueries' (query : queries) (drop m rest)
  where
    query =
      Query
        { numCities = n,
          costOfLib = cLib,
          costOfRoad = cRoad,
          possibleRoads = map makePair $ take m rest
        }
    [n, m, cLib, cRoad] = infoLine
    makePair [x, y] = (x, y)

parseInput :: String -> [Query]
parseInput inputString =
  inputString
    & lines
    & drop 1 -- don't need the number of queries
    & map (map read . words)
    & parseQueries

program :: String -> String
program = unlines . map (show . minCostForUniversalLibAccess) . parseInput

main = interact program
