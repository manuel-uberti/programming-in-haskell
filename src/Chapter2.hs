module Chapter2 where

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns -- div (sum ns) (length ns)

-- Exercise 4
last1 ns = head (reverse ns)

-- Exercise 5
init1 ns = take (length ns - 1) ns
init2 ns = reverse (tail (reverse ns))
