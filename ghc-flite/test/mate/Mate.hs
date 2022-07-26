-- Found similar _Haskell_ sources for Matthew Naylor's Reduceron at
-- https://hackage.haskell.org/package/lazysmallcheck
-- Slightly different implementation, but good starting point for a translation.
module Mate where

import Prelude hiding (id, const, min, max, abs, any, foldr, concatMap, map, fst, snd, sum, unzip, Maybe(..), null, maybe, not, and, or)

id x = x

const c x = c

inc n = n+1

dec n = n-1

min x y
  | x <= y = x
  | otherwise = y

max x y
  | x <= y = y
  | otherwise = x

abs :: Integer -> Integer
abs n
  | n <= 0 = 0-n
  | otherwise = n

plus a b = (+) a b
minus a b = (-) a b

data Maybe a = Nothing | Just a

no Nothing = True
no (Just _) = False

maybe n j Nothing = n
maybe n j (Just x) = j x

not False = True
not True = False

and False False = False
and False True = False
and True False = False
and True True = True

or False False = False
or False True = True
or True False = True
or True True = True

con True q = q
con False q = False

dis True q = True
dis False q = q

fst (x,y) = x
snd (x,y) = y

cross (f,g) (x,y) = (f x, g y)

null [] = True
null (x:xs) = False

append [] ys = ys
append (x:xs) ys = x:append xs ys

elemAt [] n = 0
elemAt (x:xs) n = case n == 0 of
  True -> x
  False -> elemAt xs (n-1)

map f [] = []
map f (x:xs) = f x : map f xs

concatMap f [] = []
concatMap f (x:xs) = append (f x) (concatMap f xs)

any p [] = False
any p (x:xs) = dis (p x) (any p xs)

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

sumAcc acc [] = acc
sumAcc acc (x:xs) = sumAcc (acc + x) xs

sum xs = sumAcc 0 xs

unzip [] = ([],[])
unzip ((a,b):xs) = let u = unzip xs
                   in (a:fst u, b:snd u)

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
data Colour = Black | White
type Piece = (Colour,Kind)
type Square = (Integer,Integer)
data Board = Board
               [(Kind,Square)] -- white
               [(Kind,Square)] -- black
data Move = Move
    Square    -- to here
    (Maybe Piece) -- capturing this
    (Maybe Piece) -- gaining promotion to this
data MoveInFull = MoveInFull Piece Square Move
data Solution = Solution MoveInFull [(MoveInFull, Solution)]

kindToChar King   = 'K'
kindToChar Queen  = 'Q'
kindToChar Rook   = 'R'
kindToChar Bishop = 'B'
kindToChar Knight = 'N'
kindToChar Pawn   = 'P'

isKing k = (kindToChar k) == 'K'

pieceAt (Board wkss bkss) sq =
  pieceAtWith sq White (pieceAtWith sq Black Nothing bkss) wkss

pieceAtWith sq c n [] = n
pieceAtWith sq c n ((k, s) : xs) =
  case sameSquare s sq of
    True -> Just (c, k)
    False -> pieceAtWith sq c n xs

emptyAtAll (Board wkss bkss) e =
        emptyAtAllAnd e (emptyAtAllAnd e True bkss) wkss

emptyAtAllAnd e b [] = b
emptyAtAllAnd e b ((k, s) : xs) =
  case e s of
    True -> False
    False -> emptyAtAllAnd e b xs

rmPieceAt White sq (Board wkss bkss) = Board (rPa sq wkss) bkss
rmPieceAt Black sq (Board wkss bkss) = Board wkss (rPa sq bkss)

rPa sq [] = []
rPa sq (ks : kss) =
  case ks of
    (k, s) -> case sameSquare s sq of
                True -> kss
                False -> ks : (rPa sq kss)

putPieceAt sq (c, k) (Board wkss bkss) =
  case c of
    White -> Board ((k, sq) : wkss) bkss
    Black -> Board wkss ((k, sq) : bkss)

kingSquare :: Colour -> Board -> (Integer, Integer)
kingSquare c b = kSq (forcesColoured c b)

kSq [] = (0,0)
kSq ((k, s) : kss) =
  case isKing k of
    True  -> s
    False -> kSq kss

opponent Black = White
opponent White = Black

colourOf (c, k) = c
kindOf   (c, k) = k

sameColour White White = True
sameColour White Black = False
sameColour Black White = False
sameColour Black Black = True

rank (f, r) = r
file (f, r) = f

sameSquare (f1, r1) (f2, r2) = con (f1 == f2) (r1 == r2)

onboard :: (Integer, Integer) -> Bool
onboard (p, q) =
  con (con (1 <= p) (p <= 8))
      (con (1 <= q) (q <= 8))

forcesColoured White (Board wkss bkss) = wkss
forcesColoured Black (Board wkss bkss) = bkss

moveDetailsFor c bd =
  concatMap (movesForPiece c bd) (forcesColoured c bd)

movesForPiece c bd p =
  concatMap (tryMove c bd p) (rawmoves c p bd)

tryMove c bd (k, sqFrom) (Move sqTo mcp mpp) =
        let p   = (c, k)
            bd1 = rmPieceAt c sqFrom bd
            pp  = maybe p id mpp
            bd2 = maybe (putPieceAt sqTo pp bd1)
                           (const (putPieceAt sqTo pp
                           (rmPieceAt (opponent c) sqTo bd1)))
                            mcp
        in case kingincheck c bd2 of
             False -> [((MoveInFull p sqFrom (Move sqTo mcp mpp)), bd2)]
             True  -> []

rawmoves c (k, sq) bd =
        let m = case k of
                  King   -> kingmoves
                  Queen  -> queenmoves
                  Rook   -> rookmoves
                  Bishop -> bishopmoves
                  Knight -> knightmoves
                  Pawn   -> pawnmoves
        in m c sq bd

bishopmoves c sq bd =
        append (moveLine bd c sq (cross (dec, inc))) (
        append (moveLine bd c sq (cross (inc, inc))) (
        append (moveLine bd c sq (cross (dec, dec)))
               (moveLine bd c sq (cross (inc, dec))) ))

rookmoves c sq bd =
        append (moveLine bd c sq (cross (dec, id))) (
        append (moveLine bd c sq (cross (inc, id))) (
        append (moveLine bd c sq (cross (id, dec)))
               (moveLine bd c sq (cross (id, inc))) ))

moveLine bd c sq inc =
        let incsq = inc sq in
        case onboard incsq of
          True -> case pieceAt bd incsq of
                    Nothing -> (Move incsq Nothing Nothing) :
                                  (moveLine bd c incsq inc)
                    Just p  -> case sameColour (colourOf p) c of
                                 False -> [(Move incsq (Just p) Nothing)]
                                 True  -> []
          False -> []

kingmoves c (p, q) bd =
  let pi = p + 1
      pd = p - 1
      qi = q + 1
      qd = q - 1
  in sift c bd []
       [(pd, qi), (p, qi), (pi, qi)
       ,(pd, q )         , (pi, q )
       ,(pd, qd), (p, qd), (pi, qd)
       ]

knightmoves c (p, q) bd =
  let pi  = p + 1
      pd  = p - 1
      qi  = q + 1
      qd  = q - 1
      pi2 = p + 2
      pd2 = p - 2
      qi2 = q + 2
      qd2 = q - 2
  in sift c bd []
       [(pd, qi2), (pi, qi2), (pd2, qi), (pi2, qi)
       ,(pd2, qd), (pi2, qd), (pd, qd2), (pi, qd2)]

sift c bd ms []           = ms
sift c bd ms (sq : sqs) =
        case onboard sq of
          False -> sift c bd ms sqs
          True  ->
            case pieceAt bd sq of
              Nothing -> sift c bd ((Move sq Nothing Nothing) : ms) sqs
              Just p  -> case sameColour (colourOf p) c of
                           True  -> sift c bd ms sqs
                           False -> sift c bd ((Move sq (Just p) Nothing) : ms) sqs

pawnmoves c (p, q) bd =
  let fwd  = case c of
               White -> 1
               Black -> 0 - 1
      on1  = (p, (q + fwd))
      on2  = (p, (q + fwd) + fwd)
      mov2 = case con (secondRank c q) (no (pieceAt bd on2)) of
               True -> [Move on2 Nothing Nothing]
               False -> []
      movs = case no (pieceAt bd on1) of
               True -> append (promote c on1 Nothing) mov2
               False -> []
      dii  = (p + 1, q + fwd)
      did  = (p - 1, q + fwd)
      caps = append (promoteCap c dii bd) (promoteCap c did bd)
  in append movs caps

promoteCap c sq bd =
  let mcp = pieceAt bd sq in
  case mcp of
    Nothing  -> []
    Just p   -> case sameColour (colourOf p) c of
                  False -> promote c sq mcp
                  True  -> []

promote c sq mcp =
  case lastRank c (rank sq) of
    True  -> map (Move sq mcp)
               [Just (c, Queen)
               ,Just (c, Rook)
               ,Just (c, Bishop)
               ,Just (c, Knight)]
    False -> [Move sq mcp Nothing]

secondRank :: Colour -> Integer -> Bool
secondRank White r = r == 2
secondRank Black r = r == 7

lastRank :: Colour -> Integer -> Bool
lastRank White r = r == 8
lastRank Black r = r == 1

queenmoves c sq bd = append (bishopmoves c sq bd) (rookmoves c sq bd)

kingincheck c bd =
        any (kingInCheckFrom c bd) (forcesColoured (opponent c) bd)

kingInCheckFrom :: Colour -> Board -> (Kind, Square) -> Bool
kingInCheckFrom c bd (f, (x, y)) =
  case kingSquare c bd :: (Integer, Integer) of
    (xk, yk) ->
      case f of
        King   -> con ((<=) (abs ((-) x xk)) 1)
                      ((<=) (abs ((-) y yk)) 1)
        Queen  -> dis (kingInCheckFrom c bd (Rook   ,(x, y)))
                      (kingInCheckFrom c bd (Bishop ,(x, y)))
        Rook   -> dis (con ((==) x xk)
                           (emptyAtAll bd (filePath xk y yk)))
                      (con ((==) y yk)
                           (emptyAtAll bd (rankPath yk x xk)))
        Bishop -> dis (con ((==) ((-) x y) ((-) xk yk))
                           (emptyAtAll bd (diagPath minus ((-) xk yk) x xk)))
                      (con ((==) ((+) x y) ((+) xk yk))
                           (emptyAtAll bd (diagPath plus ((+) xk yk) x xk)))
        Knight -> dis (con ((==) (abs ((-) x xk)) 2) ((==) (abs ((-) y yk)) 1))
                      (con ((==) (abs ((-) x xk)) 1) ((==) (abs ((-) y yk)) 2))
        Pawn   -> con ((==) (abs ((-) x xk)) 1)
                      ((==) yk (onFor c y ))

onFor Black = inc
onFor White = dec

filePath :: Integer -> Integer -> Integer -> (Integer, Integer) -> Bool
filePath xk yFrom yTo (x, y) =
  let ylo = (+) (min yFrom yTo) 1
      yhi = (-) (max yFrom yTo) 1
  in  con ((==) x xk) (con ((<=) ylo y) ((<=) y yhi))

rankPath :: Integer -> Integer -> Integer -> (Integer, Integer) -> Bool
rankPath yk xFrom xTo (x, y) =
  let xlo = (+) (min xFrom xTo) 1
      xhi = (-) (max xFrom xTo) 1
  in  con ((==) y yk) (con ((<=) xlo x) ((<=) x xhi))

diagPath :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> (Integer, Integer) -> Bool
diagPath op d xFrom xTo (x, y) =
  let xlo = (+) (min xFrom xTo) 1
      xhi = (-) (max xFrom xTo) 1
  in  con ((==) (op x y) d) (con ((<=) xlo x) ((<=) x xhi))

solve bd c n = showResult (solution bd c ((-) ((+) n n) 1))

solution bd c n  =
  let mds = moveDetailsFor c bd in
  foldr (solnOr c n) Nothing mds

solnOr c n (mif, b) other =
        case replies b (opponent c) ((-) n 1) of
          Nothing -> other
          Just rs -> case null rs of
               True -> case kingincheck (opponent c) b of
                       True  -> Just (Solution mif [])
                       False -> other
               False -> Just (Solution mif rs)

replies :: Board -> Colour -> Integer -> Maybe [(MoveInFull, Solution)]
replies bd c n =
  let mds = moveDetailsFor c bd in
  case (==) n 0 of
    True  -> case null mds of
               True -> Just []
               False -> Nothing
    False -> foldr (solnAnd c n) (Just []) mds

solnAnd c n (mif, b) rest =
  case solution b (opponent c) ((-) n 1) of
    Nothing -> Nothing
    Just s  -> case rest of
      Nothing -> Nothing
      Just ms -> Just ((mif, s) : ms)

showResult :: Maybe Solution -> Integer
showResult Nothing  = 0
showResult (Just s) = size s

size :: Solution -> Integer
size (Solution mif rs) = (+) 1 (sum (map size (snd (unzip rs))))

main =
  let problem = (Board
                  [(Knight ,(7, 8))
                  ,(Rook   ,(5, 7))
                  ,(King   ,(8, 7))
                  ,(Bishop ,(4, 5))
                  ,(Pawn   ,(8, 4))
                  ,(Pawn   ,(7, 3))
                  ,(Pawn   ,(5, 2))
                  ,(Pawn   ,(6, 2))
                  ,(Queen  ,(5, 1))]

                  [(Knight ,(2, 8))
                  ,(Pawn   ,(7, 7))
                  ,(Pawn   ,(4, 6))
                  ,(Pawn   ,(3, 5))
                  ,(King   ,(6, 5))
                  ,(Pawn   ,(8, 5))
                  ,(Pawn   ,(4, 4))
                  ,(Pawn   ,(2, 3))
                  ,(Pawn   ,(5, 3))
                  ,(Pawn   ,(7, 2))
                  ,(Queen  ,(1, 1))
                  ,(Knight ,(2, 1))
                  ,(Bishop ,(8, 1))]

                  ,(White, 3))
  in solveProblem problem

solveProblem (bd, (c, n)) = solve bd c n
