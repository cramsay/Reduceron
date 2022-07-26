module Test where

import Prelude hiding (or, length, map, null, foldr1, EQ, LT, GT)

map f [] = []
map f (x:xs) = f x : map f xs

data Outcome = Win
             | Loss
             | Draw

bestOf :: Outcome -> Outcome -> Outcome
bestOf Win v = Win
bestOf Loss v = v
bestOf Draw Win = Win
bestOf Draw Draw = Draw
bestOf Draw Loss = Draw

inverse :: Outcome -> Outcome
inverse Loss = Win
inverse Draw = Draw
inverse Win = Loss

fromTo :: Integer -> Integer -> [Integer]
fromTo n m = case n <= m of
               True -> n : fromTo (n+1) m
               False -> []

data Comp = EQ | LT | GT

cmp :: Integer -> Integer -> Comp
cmp a b = case a==b of
            True -> EQ
            False -> case a <= b of
              True -> LT
              False -> GT

insert x [] = [x]
insert x (y:ys) = case x <= y of
                    True -> x : y : ys
                    False -> y : insert x ys

foldr1 f [] = Win -- Impossible
foldr1 f [x] = x
foldr1 f (x:y:ys) = f x (foldr1 f (y:ys))

diff [] ys = []
diff (x:xs) [] = x:xs
diff (x:xs) (y:ys) =
  case cmp x y of
    LT -> x : diff xs (y:ys)
    EQ -> diff xs ys
    GT -> diff (x:xs) ys

null [] = True
null (x:xs) = False

subset xs ys = null (diff xs ys)

or False x = x
or True x = True

hasLine p =
  or (subset [1,2,3] p)
    (or (subset [4,5,6] p)
      (or (subset [7,8,9] p)
        (or (subset [1,4,7] p)
          (or (subset [2,5,8] p)
            (or (subset [3,6,9] p)
              (or (subset [1,5,9] p)
                (subset [3,5,7] p)))))))

length :: [a] -> Integer
length xs = lengthAcc 0 xs

lengthAcc :: Integer -> [a] -> Integer
lengthAcc acc [] = acc
lengthAcc acc (x : xs) = lengthAcc (1 + acc) xs;

gridFull ap pp = (length ap) + (length pp) == 9

analysis ap pp =
  case hasLine pp of
    True -> Loss
    False -> case gridFull ap pp of
      True -> Draw
      False -> foldr1 bestOf (map (moveval ap pp)
                 (diff (diff (fromTo 1 9) ap) pp))

moveval ap pp m = inverse (analysis pp (insert m ap))

data Symbol = O | X

adjudicate os xs =
  case cmp (length os) (length xs) of
    GT -> report (analysis xs os) X
    EQ -> case hasLine xs of
      True -> report Win X
      False -> case hasLine os of
        True -> report Win O
        False -> report (analysis xs os) X
    LT -> report (analysis os xs) O

report :: Outcome -> Symbol -> Integer
report Loss s = side (opp s)
report Win s = side s
report Draw p = 3

opp O = X
opp X = O
side O = 0
side X = 1

main = adjudicate [] []
