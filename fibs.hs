nextFib (first, second) = (second, first + second)

fibs = map fst $ iterate nextFib (0, 1)

nth = (!!)

fibonacci = nth fibs

fibs' = 0 : 1 : zipWith (+) fibs (tail fibs)
