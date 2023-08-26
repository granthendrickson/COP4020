multiply2 x y = x * y

multiply3 x y z = x * y * z

first_a n = [ x | x <- [1..n], (x `mod` 6 == 0) || (x `mod` 11 == 0)]

isMult6or11n n = (n `mod` 6 == 0) || (n `mod` 11 == 0)

first_b n = [ x | x <- [1..n], isMult6or11n x ]

second_a n = [ x | x <- [1..n], 
              (x == (read (reverse (show x)) :: Int)) 
              && (3 == (read (take 1 (show x)) :: Int))] 

isPalindromeThatStartsWithDigit3 n = (n == (read (reverse (show n)) :: Int)) 
                                      && (3 == (read (take 1 (show n)) :: Int))

second_b n = [ x | x <- [1..n], isPalindromeThatStartsWithDigit3 x ]