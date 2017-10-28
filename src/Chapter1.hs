module Chapter1 where

-- Exercise 3
prod [] = 1
prod (n:ns) = n * prod ns

-- Exercise 4
qrsort [] = []
qrsort (x:xs) = qrsort larger ++ [x] ++ qrsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- Exercise 5
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]

-- Changing <= into < gives: qsort [2, 2, 3, 1, 1] -> [1,2,3]
