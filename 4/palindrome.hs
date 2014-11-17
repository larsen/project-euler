isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome s = (c1 == c2) && (isPalindrome s')
  where c1 = head s
        c2 = last s
        s' = take (length s -2) $ tail s

listPalindromes :: [String]
listPalindromes = filter isPalindrome [ show $ n * m | n <- [100 .. 999], m <- [100 .. 999] ]

toInt :: String -> Integer
toInt s = read s :: Integer
