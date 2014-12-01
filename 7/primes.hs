isPrime :: Int -> Bool
isPrime n = prime n 2
  where
    prime n k =
      if n == k
          then True
          else if (n `mod` k == 0)
              then False
              else prime n ( k +1 )

-- primes = filter isPrime (2: [3,5..])

nthprime n = primes !! (n-1)

primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
  where
   sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]  
                                 -- or:  filter ((/=0).(`rem`p)) t
                   where (h,~(_:t)) = span (< p*p) xs
