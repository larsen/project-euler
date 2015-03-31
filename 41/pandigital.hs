import Data.List
import Data.Char

isPrime :: Int -> Bool
isPrime n = prime n 2
  where
    prime n k =
      if n == k
          then True
          else if (n `mod` k == 0)
              then False
              else prime n ( k +1 )

toInt :: String -> Int
toInt s = read s :: Int

pandigital :: Int -> [Int]
pandigital n = map toInt p
  where p = permutations $ map intToDigit [1..n]

solution = head $ filter isPrime $ reverse . sort $ pandigital 7
-- negli insiemi di numeri con piu` cifre non ci sono primi?
