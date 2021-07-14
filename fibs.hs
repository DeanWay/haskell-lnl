fibonacciSeries = map fst $ iterate nextFib (0, 1)
  where
    nextFib (first, second) = (second, first + second)

nth = (!!)

fibonacci = nth fibonacciSeries

fibonacciSeries' = 0 : 1 : zipWith (+) fibonacciSeries' (tail fibonacciSeries')

fibonacci' = nth fibonacciSeries'

evens (first : second : rest) = second : evens rest
evens _ = []

odds [] = []
odds items = evens . tail $ items

everyOtherFibonnaci = evens fibonacciSeries
