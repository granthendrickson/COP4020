-- COP4020 Assignment 3 - Higher ORder Functions and Modules - Grant Hendrickson

problem1 :: [Int] -> [Int]
problem1 xs = map (\x -> x * 2) xs 

problem2 :: [Int] -> [Int]
problem2 xs = filter (\x -> x `mod` 2 == 0) xs

problem3 :: [Int] -> [Int]
problem3 xs = map (\x -> x * 3) xs 

getOdds :: [Int] -> [Int]
getOdds xs = filter (\x -> x `mod` 2 == 1) xs

squareList :: [Int] -> [Int]
squareList xs = map (\x -> x * x) xs 

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

problem4 :: [Int] -> Int
problem4 xs = sumList(squareList(getOdds xs))