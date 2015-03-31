divisors :: Int -> [Int]
divisors n = [ x | x <- [1 .. n-1], n `mod` x == 0 ]

-- d :: Int -> Int
-- d n = sum $ divisors n

d :: Int -> Int
d = ((map d' [0 .. ]) !!)
  where d' n = sum $ divisors n

-- ds :: [Int] -> [Int]
-- ds = map d

amicable_couples :: [[Int]]
amicable_couples = [[x,y]
                   | x <- [1 .. 10000],
                     y <- [1 .. 10000],
                     x /= y,
                     x < y,
                     d x == y,
                     d y == x]
