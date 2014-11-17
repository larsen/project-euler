square n = n * n

sumSquares n = foldl (+) 0 $ map square [1 .. n]

squareSum n = square $ foldl (+) 0 [1 .. n]
