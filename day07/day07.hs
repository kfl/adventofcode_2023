{-# LANGUAGE Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Data.Bifunctor (first)

test =  map parse [ "32T3K 765"
                  , "T55J5 684"
                  , "KK677 28"
                  , "KTJJT 220"
                  , "QQQJA 483  "
                  ]
input = map parse . lines <$> readFile "input.txt"

data Typ = High | OnePair | TwoPairs | Three | FullHouse | Four | Five
  deriving (Eq, Show, Ord, Enum, Bounded)
type Card = Char
data Hand = Hand { cards :: [Card], values :: [Int], typ :: Typ }
  deriving (Eq, Show)
type Bid = Int
type Input = [([Card], Bid)]

instance Ord Hand where
  compare (Hand _ vals1 typ1) (Hand _ vals2 typ2)
    | typ1 /= typ2 = compare typ1 typ2
    | otherwise    = compare vals1 vals2

cardValues = Map.fromList $ [ (C.intToDigit n, n) | n <- [2..9]] ++ zip "TJQKA" [10..14]
value c = cardValues ! c

freqMap vals = foldr (Map.alter (maybe (Just 1) (pure . (+1)))) Map.empty vals

eval :: [Int] -> Typ
eval vals =
  case L.sort $ Map.elems $ freqMap vals of
    [5] -> Five
    [1,4] -> Four
    [2,3] -> FullHouse
    [1,1,3] -> Three
    [1,2,2] -> TwoPairs
    [1,1,1,2] -> OnePair
    _ -> High

parse :: String -> ([Card], Bid)
parse str = (cards, read bid)
  where [cards, bid] = words str

cardsToHand cards = Hand cards vals typ
  where vals = map value cards
        typ = eval vals

part1 :: Input -> Int
part1 input = sum winnings
  where winnings = zipWith (*) [1..] $ map snd $ L.sortOn fst $ map (first cardsToHand) input
answer1 = part1 <$> input

value2 c | c == 'J' = 1
         | otherwise = value c

bestPossibleType vals = typ $ maximum replaced
  where replaced = [ Hand "" vals (eval vals')
                   | i <- [2..14], i /= 11,
                     let vals' = map (\c -> if c == 1 then i else c) vals
                   ]

cardsToHand2 cards = Hand cards vals typ
  where vals = map value2 cards
        typ = bestPossibleType vals

part2 :: Input -> Int
part2 input = sum winnings
  where winnings = zipWith (*) [1..] $ map snd $ L.sortOn fst $ map (first cardsToHand2) input

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
