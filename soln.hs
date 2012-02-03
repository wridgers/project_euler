-- Project Euler

problem1 :: Integer
problem1 = sum [ x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

problem2 :: Integer
problem2 = sum [ x | x <- takeWhile (< 4000000) fibs, x `mod` 2 == 0]

-- infinte list of fibs
fibs :: [Integer]
fibs = fib' [1,0]
fib' :: [Integer] -> [Integer]
fib' (x:y:xs) = y:(fib' ((x+y):x:xs))

problem3 :: Integer
problem3 = 0

-- list of factors
factors :: Integer -> [Integer]
factors n = fact' [n]
fact' :: [Integer] -> [Integer]
fact' (x:xs) = x:xs

-- isPrime
isPrime :: Integer -> Bool
isPrime n  
    | n < 1                 = False
    | n < 4                 = True
    | n `mod` 2 == 0        = False
    | otherwise             = isPrime' 3 n
isPrime' :: Integer -> Integer -> Bool
isPrime' a r 
    | a >= r                = True
    | r `mod` a == 0        = False
    | otherwise             = isPrime' (a+2) r

-- infinite list of primes
primes :: [Integer]
primes = [ x | x <- [1..], isPrime(x) ]
