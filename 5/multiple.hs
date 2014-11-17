sumRem k n = foldl (+) 0 $ [ n `rem` m | m <- [ 1 .. k ] ]

sumRem20 = sumRem 20

sumRem10 = sumRem 10

sumList = map abbinamento [1..]
  where abbinamento n = (n, sumRem20 n)

solution = head $ filter ezero sumList
  where ezero c = snd c == 0

# Solution is 232792560
