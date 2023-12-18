{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L

test =  map parse [ "R 6 (#70c710)"
                  , "D 5 (#0dc571)"
                  , "L 2 (#5713f0)"
                  , "D 2 (#d2c081)"
                  , "R 2 (#59c680)"
                  , "D 2 (#411b91)"
                  , "L 5 (#8ceee2)"
                  , "U 2 (#caa173)"
                  , "L 1 (#1b58a2)"
                  , "U 2 (#caa171)"
                  , "R 2 (#7807d2)"
                  , "U 3 (#a77fa3)"
                  , "L 2 (#015232)"
                  , "U 2 (#7a21e3)"
                  ]
input = map parse . lines <$> readFile "input.txt"

data Direction = U | D | L | R
  deriving (Eq, Show, Read, Ord, Enum)
type Color = String
type Instruction = (Direction, Int, Color)

type Input = [Instruction]

parse :: String -> Instruction
parse str = (read d, read n, filter (`notElem` "()#") c)
  where [d, n, c] = words str

dirToPair = \case U -> (0,-1); D -> (0,1); L -> (-1,0); R -> (1,0)

move (x, y) dir n = (x+n*a, y+n*b)
  where (a, b) = dirToPair dir

dig :: [Instruction] -> ((Int, Int), [(Int, Int)], Int)
dig = L.foldl' step ((0,0), [], 0)
  where step (curr, path, acc) (dir, n, _) = (move curr dir n, curr : path, acc + n)


-- Use the Shoelace formula to compute the area of the dug out area, remember to add the border
area :: Input -> Int
area insts = (len + abs(trapezoid final prim + acc)) `div` 2 + 1
  where (_, prim : rest, len) = dig insts
        (final, acc) = L.foldl' (\(prev, acc) p -> (p, acc + trapezoid prev p)) (prim, 0) rest
        trapezoid (x1,y1) (x2,y2) = (y1+y2) * (x1-x2)

part1 :: Input -> Int
part1 = area
answer1 = part1 <$> input

readHex :: String -> Int
readHex = L.foldl' (\acc x -> acc * 16 + C.digitToInt x) 0

digitToDir = \case '0' -> R; '1' -> D; '2' -> L; '3' -> U

parse2 (_, _, hex) = (digitToDir d, readHex n, "")
  where (n, [d]) = splitAt 5 hex

part2 :: Input -> Int
part2 input = part1 $ map parse2 input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
