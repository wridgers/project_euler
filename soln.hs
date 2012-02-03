-- Project Euler

import Data.List

-- Problem 1
problem1 :: Integer
problem1 = sum [ x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- Problem 2
problem2 :: Integer
problem2 = sum [ x | x <- takeWhile (< 4000000) fibs, x `mod` 2 == 0]

-- infinte list of fibs
fibs :: [Integer]
fibs = fib' [1,0]
fib' :: [Integer] -> [Integer]
fib' (x:y:xs) = y:(fib' ((x+y):x:xs))

-- Problem 3
problem3 :: Integer
problem3 = last (sort (factors 600851475143))

-- list of factors
factors :: Integer -> [Integer]
factors n = fact' [n]
fact' :: [Integer] -> [Integer]
fact' (x:xs)
        | isPrime(x)        = x:xs
        | otherwise         = fact' ((fact'' 2 x) ++ xs)
fact'' :: Integer -> Integer -> [Integer]
fact'' a d
        | not (isPrime(a))  = fact'' (a+1) d
        | d `mod` a == 0    = (d `div` a):a:[]
        | otherwise         = fact'' (a+1) d

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

-- Problem 4
problem4 :: Integer
problem4 = last (sort [ x*y | x <- [100..999], y <- [100..999], palendrome (show (x*y))])

palendrome :: String -> Bool
palendrome x
    | length x == 1 || x == []      = True
    | check                         = palendrome centre
    | not check                     = False
    where
        centre = init ( tail x )
        check = head x == last x

-- Problem 5
problem5 :: Integer
problem5 = problem5' 2 20
problem5' :: Integer -> Integer -> Integer
problem5' a b
    | a == 21               = b
    | b `mod` a == 0        = problem5' (a+1) b
    | b `mod` a /= 0        = problem5' 2 (b+20)

-- Problem 6
problem6 :: Integer
problem6 = (sum [ x | x<- [1..100]])^2 - sum([ x^2 | x <- [1..100]])

-- Problem 7
problem7 :: Integer
problem7 = last ( take 10002 primes )

-- Problem 8
problem8 :: Integer
problem8 = 0

-- Problem 9
problem9 :: Integer
problem9 = head [ a*b*c | a <- [1..1000], b <- [1..1000], c <- [1..1000], 
                    a + b + c == 1000, a^2 + b^2 == c^2, a < b, b < c] 

-- Problem 10
problem10 :: Integer
problem10 = 0







