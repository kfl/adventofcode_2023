{-# LANGUAGE Strict #-}
module Main where

import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.MemoUgly as Ugly


test =  map parse [ "???.### 1,1,3"
                  , ".??..??...?##. 1,1,3"
                  , "?#?#?#?#?#?#?#? 1,3,1,6"
                  , "????.#...#... 4,1,1"
                  , "????.######..#####. 1,6,5"
                  , "?###???????? 3,2,1"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Condition = String
type Checksum = [Int]
type ConditionRecord = (Condition, Checksum)
type Input = [ConditionRecord]


parse :: String -> ConditionRecord
parse str = (cond, checksum)
  where [cond, checks] = words str
        checksum = read $ "[" ++ checks ++ "]"

possible :: (Int, Checksum) -> Condition -> Int
possible acc [] =
  case acc of
    (0, [])           -> 1
    (x, [y]) | x == y -> 1
    _                 -> 0
possible acc ('.' : rest) =
  case acc of
    (x, y : check) | x == y -> possible (0, check) rest
    (0, check)              -> possible (0, check) rest
    _                       -> 0
possible acc ('#' : rest) =
  case acc of
    (x, y : check) | x < y  -> possible (x+1, y : check) rest
    (0, check)              -> possible (1, check) rest
    _                       -> 0
possible acc ('?' : rest) =
  possible acc ('.' : rest) + possible acc ('#' : rest)

possibleArrangements (condition, check) = possible (0, check) condition

part1 :: Input -> Int
part1 input = sum $ map possibleArrangements input
answer1 = part1 <$> input

enlarge factor input = map (\(co, ch) -> (L.intercalate "?" $ L.replicate factor co,
                                          concat $ L.replicate factor ch)) input

count (cond, check) = (length $ filter (== '?') cond, length check)

part2 :: Input -> Int
part2 input = sum $ map mpossibleArrangements $ enlarge 5 input
  where
    mpossibleArrangements (condition, check) = mpossible (0, check, condition)
      where
        mpossible = Ugly.memo mpossible'
        mpossible' (0, [], [])           = 1
        mpossible' (x, [y], []) | x == y = 1
        mpossible' (x, y : check, '.' : rest) | x == y = mpossible' (0, check, rest)
        mpossible' (0,     check, '.' : rest)          = mpossible' (0, check, rest)
        mpossible' (x, y : check, '#' : rest) | x < y  = mpossible' (x+1, y : check, rest)
        mpossible' (0,     check, '#' : rest)          = mpossible' (1, check, rest)
        mpossible' (running, check, '?' : rest) =
          mpossible (running, check, '.' : rest) + mpossible (running, check, '#' : rest)
        mpossible' (_, _, _) = 0

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
