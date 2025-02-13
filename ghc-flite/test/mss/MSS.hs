module Test where

import Prelude hiding (init,max,concatMap, append, map, sum, maximum)

init :: [a] -> [a]
init [] = []
init [x] = []
init (x:y:xs) = x : (init $ y:xs)

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs@(y:ys) = xs : (inits (init xs))

tails [] = []
tails (x:xs) = (x:xs) : tails xs

map f [] = []
map f (x:xs) = f x : map f xs

append [] ys = ys
append (x:xs) ys = x : append xs ys

concatMap f [] = []
concatMap f (x:xs) = append (f x) (concatMap f xs)

segments xs = concatMap tails (inits xs)

maximum [] = 0
maximum (x:xs) = max x xs

max m [] = m
max m (x:xs) = case m <= x of
                  True -> max x xs
                  False -> max m xs

sum xs = sumAcc 0 xs
sumAcc acc [] = acc
sumAcc acc (x:xs) = sumAcc (x+acc) xs

mss xs = maximum (map sum (segments xs))

fromTo n m = case n <= m of
               True -> n : fromTo (n+1) m
               False -> []

main = (mss (fromTo (0-160) 160) -
        mss (fromTo (0-150) 150)) +
       (mss (fromTo (0-161) 161) -
        mss (fromTo (0-151) 151))
