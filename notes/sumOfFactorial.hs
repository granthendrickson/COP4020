-- COP4020 Notes 9/12/2023
import Data.Char

-- Get the sum of all the digits of the factorial of an int
factorial 0 = 1
factorial n = n * factorial(n-1)

numberToIntList n = map digitToInt (show n)

sumOfFactorialDigits n = sum (numberToIntList (factorial n))

------------------------------------
{-

A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors of
28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
number.

A number n is called deficient if the sum of its proper divisors is less
than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123
can be written as the sum of two abundant numbers. However, this upper
limit cannot be reduced any further by analysis even though it is known that
the greatest number that cannot be expressed as the sum of two abundant
numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum
of two abundant numbers.

-}
getDivisors n = [ x | x <- [1..(n-1)], n `mod` x == 0]

isAbundant n = (sum (getDivisors n)) > n

listAbundants n = [ x | x <- [1..n], isAbundant x]

isSumOfTwoAbundants n = go n (listAbundants n) 0 0
  where
  go n xs f s
    | f >= (length xs) = False
    | s >= (length xs) = go n xs (f+1) 0
    | not (isAbundant (xs !! f)) = go n xs f (s+1)
    | not (isAbundant (xs !! s)) = go n xs f (s+1)
    | (xs !! f)+(xs !! s)==n = True
    | otherwise = go n xs f (s+1)

solveProblem n = sum [ x | x<-[1..n],not (isSumOfTwoAbundants x)]

----------------------------------------
{-

Create a function that goes from 1 to n creating a list of numbers that are both
Fibonacci and prime.

-}

isPrime n = ip n [2..((isqrt (n)))]
  where
  ip _ [] = True
  ip n (x:xs)
    | n `mod` x == 0 = False
    | otherwise = ip n xs

isqrt :: Integral i => i -> i
isqrt = floor . sqrt . fromIntegral
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibList n = [1,1] ++ (go n 1 1 1)
  where
  go n c f s
    | (c+2) > n = []
    | otherwise = (f+s) : go n (c+1) s (f+s)

solveProblem' n xs = [ x | x <- [2..n], isPrime x, any (x==) xs ]

--------------------------------------------
{-

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million. (Problem 10)

-}

listPrimes n = [x | x<-[2..n], isPrime x]

sumOfPrimesBelow n = sum (listPrimes n)


----------------------------------------------

main :: IO()
main = do
  print(sumOfPrimesBelow 100)