module Chapter4 where

-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-- Exercise 2
third :: [a] -> a
third xs =
  if length xs >= 3
    then head (tail (tail xs))
    else error "List too short"
-- third xs = xs !! 2
-- third (_:_:x:_) = x

-- Exercise 3
safetail :: [a] -> [a]
-- safetail xs = if null xs
--               then xs
--               else tail xs
-- safetail xs | null xs = xs
--             | otherwise = tail xs
safetail [] = []
safetail (_:xs) = xs

-- Exercise 4
(||) :: Bool -> Bool -> Bool
-- 1
True || True = True
True || False = True
False || True = True
False || False = False
-- 2
-- False || False = False
-- _ || _ = True
-- 3
-- True || _ = True
-- False || b = b
-- 4
-- b || c | b == c = b
--        | otherwise = True

-- Exercise 5
-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _ && _ = False
-- a && b = if a == True then
--           if b == True then
--             True
--           else False
--         else False

-- Exercise 6
-- (&&) :: Bool -> Bool -> Bool
-- True && b = b
-- False && _ = False
-- a && b = if a == True then b else False

-- Exercise 7
mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z
mult = \x -> (\y -> (\z -> x*y*z))

-- Exercise 8
double :: Int -> Int
double x = x + x

luhnDouble :: Int -> Int
luhnDouble x =
  if n > 9
    then n - 9
    else n
  where
    n = double x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =
  if sum `mod` 10 == 0
    then True
    else False
  where
    sum = a + b + c + d
