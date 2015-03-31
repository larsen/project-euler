isDigit c = c >= '0' && c <= '9'

digitToInt :: Char -> Int
digitToInt c
  | isDigit c            =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
  | otherwise            =  error "Char.digitToInt: not a digit"

pow :: Integer -> Integer -> Integer
pow n 1 = n
pow n e = n * pow n (e - 1)

solution = sum $ map digitToInt $ show $ pow 2 1000
