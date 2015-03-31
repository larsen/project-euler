fib = ((map fib' [0 ..]) !!)
    where
      fib' 0 = 0
      fib' 1 = 1
      fib' n = fib (n - 1) + fib (n - 2)

fibL :: Int -> (Integer, Int)
fibL n = (f, length $ show f)
  where f = fib n

