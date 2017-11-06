module Chapter5 where

-- Exercise 1
-- sum [x^2 | x <- [1..100]]

-- Exercise 2
grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x',y') | x' <- [0..x], y' <- [0..y]]

-- Exercise 3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n , x /= y]

-- Exercise 4
myReplicate :: Int -> a -> [a]
myReplicate n a = [a | _ <- [1..n]]

-- Exercise 5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Exercise 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

perfect :: Int -> Bool
perfect n = n == sum [x | x <- factors n, x /= n]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

-- Exercise 7
-- [(x,y) | x <- [1,2], y <- [3,4]] => [(1,3),(1,4),(2,3),(2,4)]
-- concat [[(x,y) | x <- [1,2]] | y <- [3,4]] => [(1,3),(2,3),(1,4),(2,4)]

-- Exercise 8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

myPositions :: Eq a => a -> [a] -> [Int]
myPositions x xs = find x (zip xs [0..])

-- Exercise 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [(x * y) | (x,y) <- zip xs ys]
