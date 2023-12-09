{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-x-partial #-} -- I'm using `head` and `tail`, I know what I'm doing
module Main where

test =  map parse [ "0 3 6 9 12 15"
                  , "1 3 6 10 15 21"
                  , "10 13 16 21 30 45"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Input = [[Int]]

parse :: String -> [Int]
parse str = map read $ words str

diffs xs = zipWith (-) (tail xs) xs

fullHistory xs = takeWhile (any (/= 0)) $ iterate diffs xs

part1 :: Input -> Int
part1 input = sum predictions
  where predictions = map (sum . map last . fullHistory) input

answer1 = part1 <$> input

part2 :: Input -> Int
part2 input = sum predictions
  where predictions = map (diff . map head . fullHistory) input
        diff = foldr1 (-)

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
