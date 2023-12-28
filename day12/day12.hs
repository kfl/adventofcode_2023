{-# LANGUAGE Strict, OverloadedLists #-}
module Main where

import qualified Data.List as L
import qualified Data.List.Split as L

import Control.Monad.Memo qualified as Memo
import Data.Vector.Unboxed qualified as U
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Control.Monad.Par (runPar, parMap)


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

enlarge factor = map (\(co, ch) -> (L.intercalate "?" $ L.replicate factor co,
                                    concat $ L.replicate factor ch))

count (cond, check) = (length $ filter (== '?') cond, length check)

part2 :: Input -> Int
part2 input = sum $ runPar $ parMap arrangements large
  where
    large = enlarge 5 input
    arrangements (condition, check) = Memo.startEvalMemo $ mpossible (condition, U.fromList check)
    mpossible = Memo.memo mpossible'
    mpossible' (cs, ns)
      | U.null ns, all (`elem` ".?") cs = pure 1
      | U.null ns = pure 0
      | null cs = pure 0
      | '.' : cs <- cs = mpossible' (cs, ns)
      | '#' : cs <- cs, Just (n, ns) <- U.uncons ns =
          case splitAt (n-1) cs of
            (a, c:b) | length a == n-1, all (`elem` "#?") a, c `elem` "?." -> mpossible' (b, ns)
            (a, []) | length a == n-1, all (`elem` "#?") a -> mpossible' ([], ns)
            _ -> pure 0
      | '?' : cs <- cs = (+) <$> mpossible ('.' : cs, ns) <*> mpossible ('#' : cs, ns)


answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
