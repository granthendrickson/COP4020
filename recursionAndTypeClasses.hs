-- COP4020 Assignment 2 - Recursion and Type Classes - Grant Hendrickson

isPrime :: Int -> Bool
isPrime n = ip n [2..isqrt n]
  where
    ip _ [] = True
    ip n (x:xs)
      | n `mod` x == 0 = False
      | otherwise = ip n xs

isqrt :: Integral i => i -> i
isqrt = floor . sqrt . fromIntegral

getSquareRoot :: Int -> Double
getSquareRoot x = (fromIntegral x ** 0.5)

getIntSquareRoot :: Double -> Int
getIntSquareRoot x = floor (x ** 0.5)

hasThreeFactors :: Int -> Bool
hasThreeFactors x = if isInt (getSquareRoot x) then isPrime (getIntSquareRoot (fromIntegral x)) else False

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x)

keep [] = []
keep (x:xs) = x:remove xs

remove [] = []
remove (x:xs) = keep xs

problem1 :: Int -> [Int]
problem1 n = keep (take (n * 2) [x | x <- [2, 3..], isPrime x])

problem2 :: Integer -> [Integer]
problem2 n = filter (\x -> x `mod` 10 == 3) (takeWhile (<= n) fibs)
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem3 :: Int -> [Int]
problem3 n = [x | x <- [2..n], x `mod` 5 == 0 || hasThreeFactors x]

