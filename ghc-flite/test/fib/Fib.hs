module Test where

main = fib 20

fib :: Integer -> Integer
fib n
  | n <= 1 = 1
  | otherwise = fib (n-2) + fib (n-1)
