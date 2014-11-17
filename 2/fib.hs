-- see also Binet's formula
-- http://mathworld.wolfram.com/BinetsFibonacciNumberFormula.html

-- Memoized Fibonacci
-- http://www.kimbly.com/blog/000263.html

fib = ((map fib' [0 ..]) !!)
    where
      fib' 0 = 0
      fib' 1 = 1
      fib' n = fib (n - 1) + fib (n - 2)

-- http://blog.whoop.as/?p=1668

let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- foldr (+) 0 (filter even (map fib [ 1 .. 30 ]))
