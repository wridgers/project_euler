-- Project Euler

problem1 :: Integer
problem1 = sum [ x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

problem2 :: Integer
problem2 = sum [ x | x <- takeWhile (< 4000000) fibs, x `mod` 2 == 0]

-- infinte list of fibs
fibs = fib' [1,0]
fib' :: [Integer] -> [Integer]
fib' (x:y:xs) = y:(fib' ((x+y):x:xs))

problem3 :: Integer
problem3 = 0

-- list of factors


-- isPrime

