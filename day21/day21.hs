{-# LANGUAGE Strict, OverloadedLists #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- import qualified Data.Set as Set
-- import Data.Set (Set)

import qualified Data.HashSet as Set
import Data.HashSet (HashSet)

import Data.Sequence (Seq (..), (><), (|>), ViewR(..))
import qualified Data.Sequence as Seq



test = parse [ "..........."
             , ".....###.#."
             , ".###.##..#."
             , "..#.#...#.."
             , "....#.#...."
             , ".##..S####."
             , ".##..#...#."
             , ".......##.."
             , ".##.#.####."
             , ".##..##.##."
             , "..........."
             ]
input = parse . lines <$> readFile "input.txt"

type Set = HashSet

type Pos = (Int, Int)
type Input = (Set Pos, Pos, Int, Int)

parse :: [String] -> Input
parse rows@(cols : _) = (Set.fromList rocks, start, length cols, length rows)
  where rocks = [ (x,y) | (y, row) <- zip [0..] rows
                        , (x, '#') <- zip [0..] row]
        [start] = [ (x,y) | (y, row) <- zip [0..] rows
                          , (x, 'S') <- zip [0..] row]

neighbours (i, j) = [ (i-1, j), (i+1, j), (i, j-1), (i, j+1) ]

step rocks possible = Set.difference (Set.unions $ map move $ Set.toList possible) rocks
  where move pos = Set.fromList $ neighbours pos


part1 :: Input -> Int
part1 (rocks, start, _, _) = Set.size $ steps !! 64
  where steps = iterate (step rocks) [start]
answer1 = part1 <$> input


step2 :: Int -> Int -> Set Pos -> Set (Int, Int, Int, Int) -> Set (Int, Int, Int, Int)
step2 bx by rocks possible = Set.fromList $ concatMap move $ Set.toList possible
  where move (x, y, tx, ty) = [ (x', y', tx', ty') | (i, j) <- neighbours (x,y)
                                                   , let (x', tx') = norm bx tx i
                                                         (y', ty') = norm by ty j
                                                   , not $ Set.member (x',y') rocks
                                                   ]
        norm ba ta (-1) = (ba, ta-1)
        norm ba ta a | a == ba = (0, ta+1)
                     | otherwise = (a, ta)

places (rocks, (x,y), bx, by) i = Set.size $ steps !! i
  where steps = iterate (step2 bx by rocks) [(x,y,0,0)]


-- prediction from day 9, but with sequences
slast (_ :|> x) = x
stail (_ :<| xs) = xs
diffs xs = Seq.zipWith (-) (stail xs) xs
predict xs = ps
  where ps = sum $ map slast $ takeWhile (any (/= 0)) $ iterate diffs xs

extend xs = xs |> predict xs

part2BruteForce input = places input 26_501_365

part2 :: Input -> Int
part2 input@(_, (x,_), bx, _) = slast p
  where firstPredictions = Seq.fromList [ places input (x + i*bx) | i <- [0..2]]
        predictions = iterate extend firstPredictions
        plen = (26501365 - x) `div` bx - 1
--        plen = 10

        Just p = L.find ((plen ==) . Seq.length) predictions

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
