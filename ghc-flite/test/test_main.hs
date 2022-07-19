module Test where

main :: Int
main = fib 20


fib n
  | n <= 1 = 1
  | otherwise = fib (n-2) + fib (n-1)
