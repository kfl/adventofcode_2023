{-# LANGUAGE Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (toList)


test =  parse [ "467..114.."
              , "...*......"
              , "..35..633."
              , "......#..."
              , "617*......"
              , ".....+.58."
              , "..592....."
              , "......755."
              , "...$.*...."
              , ".664.598.."
              ]
input = parse . lines <$> readFile "input.txt"

type Key = (Int, Int)
type Signature = (Int, [Int])
data Value = Number Int Signature | Symbol Char
  deriving (Eq, Show, Ord)
type Schematics = Map Key Value
type Input = Schematics

parse :: [String] -> Input
parse inp = sch
  where
    (_, sch) = L.foldl' (\(i, sch) r ->
                           let (_, j, sch', js, cur) = L.foldl' row (i, 0, sch, [], 0) r
                           in (i+1, insertNum i cur sch' js))
                        (0, Map.empty) inp

    insertNum i num sch' js =
      foldr (\j sch -> Map.insert (i, j) (Number num (i,js)) sch) sch' js

    row (i, j, sch, js, cur) c =
      if C.isDigit c then (i, j+1, sch, j:js, cur * 10 + C.digitToInt c)
      else let sch1 = insertNum i cur sch js
               sch2 = if c == '.' then sch1
                      else Map.insert (i, j) (Symbol c) sch1
            in (i, j+1, sch2, [], 0)

neighbours (i, j) = [(i-1, j-1), (i-1, j), (i-1, j+1),
                     (i  , j-1)          , (i  , j+1),
                     (i+1, j-1), (i+1, j), (i+1, j+1)]

nextToSymbol sch = Set.fromList nexts
  where syms = [ s | (s, Symbol _) <- Map.assocs sch]
        nexts = [ n | s <- syms, k <- neighbours s,
                  n@(Number _ _) <- toList $ Map.lookup k sch]


part1 :: Input -> Int
part1 input = sum [ n | Number n _ <- nexts ]
  where nexts = toList $ nextToSymbol input
answer1 = part1 <$> input

gearRatios sch = ratios
  where syms = [ s | (s, Symbol '*') <- Map.assocs sch]
        ratios = [ product [ n | Number n _ <- toList gs]
                 | s <- syms
                 , let gs = Set.fromList [ n | k <- neighbours s
                                         , n@(Number _ _) <- toList $ Map.lookup k sch]
                 , Set.size gs == 2 ]

part2 :: Input -> Int
part2 input = sum $ gearRatios input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
