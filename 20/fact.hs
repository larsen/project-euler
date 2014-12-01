fact 0 = 1
fact n = n * fact (n-1)

slide :: Int -> [a] -> [[a]]
slide _ [] = []
slide n xs = [take n xs] ++ slide n (drop 1 xs)

rInteger :: String -> Integer
rInteger = read

solution = sum $ map rInteger $ slide 1 $ show $ fact 100
