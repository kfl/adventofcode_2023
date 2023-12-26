{-# LANGUAGE LambdaCase, OverloadedLists #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import Data.Set (Set)

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

moves n rocks pos = L.filter valid $ neighbours pos
  where valid (x,y) = Set.notMember (x `mod` n, y `mod` n) rocks

step2 n rocks possible = Set.unions $ map (Set.fromList . moves n rocks) $ Set.toList possible

stepTwice n rocks (past, frontier) = (news `Set.union` past, news)
  where mov = Set.fromList . moves n rocks
        news = Set.filter (`Set.notMember` past) $ foldMap mov $ foldMap mov frontier

placesSlow (rocks, start@(x,y), bx, by) i = Set.size $ steps !! i
  where steps = iterate (step2 bx rocks) [start]

places (rocks, start@(x,y), bx, by) i = Set.size $ steps !! i
  where twiceStepper p1 p2 = p1 : p2 : twiceStepper (stepTwice bx rocks p1) (stepTwice bx rocks p2)
        afterFirst = step2 bx rocks [start]
        steps = map fst $ twiceStepper ([start], [start]) (afterFirst, afterFirst)

part2BruteForce input = places input 26_501_365

-- prediction from day 9, but with sequences
slast (_ :|> x) = x
stail (_ :<| xs) = xs
diffs xs = Seq.zipWith (-) (stail xs) xs
predict xs = ps
  where ps = sum $ map slast $ takeWhile (any (/= 0)) $ iterate diffs xs

extend xs = xs |> predict xs

part2Predict input@(_, (x,_), bx, _) = slast p
  where
    firstPredictions = Seq.fromList [ places input (x + i*bx) | i <- [0..2]]
    predictions = iterate extend firstPredictions
    plen = (26501365 - x) `div` bx
    Just p = L.find ((plen ==) . Seq.length) predictions

part2 :: Input -> Int
part2 input@(_, (x,_), bx, _) = a*n*n + b*n + c
  where
    -- We fit a 2nd degree polynomial f(n)
    -- System of equations:
    -- f(0) = a*0**2 + b*0 + c = f0, so c = f0
    -- f(1) = a*1**2 + b*1 + c = f1, so  a +  b = f1 - f0
    -- f(2) = a*2**2 + b*2 + c = f2, so 4a + 2b = f2 - f0
    -- Gauss elimination gives:         2a      = f2 - f0 - 2*(f1 - f0) = f2 - 2f1 + f0
    -- This gives:                            b = f1 - f0 - a
    f i = places input i
    (f0, f1, f2) = (f x, f (x + bx), f (x + 2*bx))
    c = f0
    a = (f2 - 2*f1 + f0) `div` 2
    b = f1 - f0 - a

    n = 26_501_365 `div` bx

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
--  print $ part2Predict inp
--  print $ part2BruteForce inp
