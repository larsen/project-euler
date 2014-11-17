-- 317584931803
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

factor :: Integral a => a -> [a]
factor n = filter (congruent n) primes
  where primes = filter isPrime [ 2, 3 .. (round $ sqrt (fromIntegral n)) +1 ]

congruent :: Integral a => a -> a -> Bool
congruent n m = n `mod` m == 0

-- per ogni k < n,
-- return 0 if n | k

isPrime n = prime n 2

prime :: Integral a => a -> a -> Bool
prime n k =
    if n == k
        then True
        else if (n `mod` k == 0)
            then False
            else prime n ( k +1 )
