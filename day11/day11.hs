{-# LANGUAGE Strict #-}
module Main where

import qualified Data.List as L

test = parse [ "...#......"
             , ".......#.."
             , "#........."
             , ".........."
             , "......#..."
             , ".#........"
             , ".........#"
             , ".........."
             , ".......#.."
             , "#...#....."
             ]
input = parse . lines <$> readFile "input.txt"

type Galaxy = (Int, Int)
type Image = [Galaxy]
type Input = Image

parse :: [String] -> Input
parse strs = [ (x, y) | (y, row) <- zip [0..] strs
                      , (x, '#') <- zip [0..] row]

expandable image = ([0 .. maximum xs] L.\\ xs, [0 .. maximum ys] L.\\ ys)
  where (xs, ys) = unzip image

adjust :: Image -> Image
adjust image = [ (x + extrax, y + extray) | (x,y) <- image,
                 let extrax = length $ filter (< x) expandX
                     extray = length $ filter (< y) expandY]
  where (expandX, expandY) = expandable image

pairs :: [a] -> [(a,a)]
pairs xs = [ (x, y) | (x:ys) <- L.tails xs, y <- ys ]

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a,b) (x,y) = abs(a-x) + abs(b-y)

part1 :: Input -> Int
part1 image = sum dists
  where adjusted = adjust image
        dists = map (uncurry manhattan) $ pairs adjusted
answer1 = part1 <$> input

adjust2 :: Int -> Image -> Image
adjust2 factor image = [ (x + extrax, y + extray) | (x,y) <- image,
                         let extrax = extra x expandX
                             extray = extra y expandY]
  where (expandX, expandY) = expandable image
        extra n expand = (factor-1) * length (filter (< n) expand)

adjustablePart factor image = sum dists
  where adjusted = adjust2 factor image
        dists = map (uncurry manhattan) $ pairs adjusted

part2 :: Input -> Int
part2 = adjustablePart 1_000_000
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
