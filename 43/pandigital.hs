import Data.List
import Data.Char
import Text.Printf

toInt :: String -> Int
toInt s = read s :: Int

pandigital :: Int -> [Int]
pandigital n = map toInt p
  where p = permutations $ map intToDigit [0..n]

-- Returns n-th dig from a number
digit :: Int -> Int -> Int
digit number n = digitToInt $ digits !! (n - 1)
  where digits = printf "%010d" number

concatDigits :: Int -> [Int] -> Int
concatDigits number idx = read s :: Int
  where s = map (intToDigit . digit number) idx

property :: Int -> Bool
property number = concatDigits number [2,3,4] `rem` 2 == 0
                  && concatDigits number [3,4,5] `rem`  3 == 0
                  && concatDigits number [4,5,6] `rem`  5 == 0
                  && concatDigits number [5,6,7] `rem`  7 == 0
                  && concatDigits number [6,7,8] `rem` 11 == 0
                  && concatDigits number [7,8,9] `rem` 13 == 0
                  && concatDigits number [8,9,10] `rem` 17 == 0

solution = sum $ filter property $ pandigital 9
