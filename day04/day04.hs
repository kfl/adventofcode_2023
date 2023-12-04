{-# LANGUAGE TupleSections, Strict #-}
module Main where

import qualified Data.List.Split as S
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

test =  map parse [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                  , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                  , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                  , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                  , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                  , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Winners = [Int]
type Haves = [Int]
type Card = (Winners, Haves)
type Input = [Card]

parse :: String -> Card
parse str = (ints ws, ints hs)
  where [_, nums] = S.splitOn ":" str
        [ws, hs] = S.splitOn "|" nums
        ints = map read . words


matches (wins, has) = Set.size common
  where
    common = Set.intersection (Set.fromList wins) (Set.fromList has)

points card = if m == 0 then 0 else 2^(m-1)
  where
    m = matches card

part1 :: Input -> Int
part1 input = sum $ map points input
answer1 = part1 <$> input

addCopies 0 _ rest = rest
addCopies i m ((c,n) : rest) = (c, m+n) : addCopies (i-1) m rest

game acc [] = acc
game acc ((card, n) : cs) = game (acc+n) cs'
  where cs' = addCopies m n cs
        m = matches card

part2 :: Input -> Int
part2 input = game 0 $ map (, 1) input
answer2 = part2 <$> input

part2Alternative input = result
  where copies = repeat 1
        addCopies i m ns = (map (m+) pre) ++ post
          where (pre, post) = splitAt i ns
        game (n : copies, acc) card = (addCopies m n copies, acc+n)
          where m = matches card
        (_, result) = foldl game (copies, 0) input


main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
