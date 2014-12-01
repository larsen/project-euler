zapWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zapWith f xs [] = xs
zapWith f [] ys = ys
zapWith f (x:xs) (y:ys) = f x y : zapWith f xs ys

extendWith f [] = []
extendWith f xs@(x:ys) = x : zapWith f xs ys

pascal = iterate (extendWith (+)) [1]

solution = maximum $ last $ take 41 pascal
