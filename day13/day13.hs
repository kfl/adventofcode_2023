{-# LANGUAGE Strict #-}
module Main where

import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Bit as B
import qualified Data.Vector.Unboxed as U
import Data.Bits (xor)
import Data.Bifunctor (bimap, first)
import Data.Maybe (fromMaybe)

test =  parse $ unlines [ "#.##..##."
                        , "..#.##.#."
                        , "##......#"
                        , "##......#"
                        , "..#.##.#."
                        , "..##..##."
                        , "#.#.##.#."
                        , ""
                        , "#...##..#"
                        , "#....#..#"
                        , "..##..###"
                        , "#####.##."
                        , "#####.##."
                        , "..##..###"
                        , "#....#..#"
                        ]
input = parse <$> readFile "input.txt"

type Pattern = (Int, Int, [(Int, Int)])
type Input = [Pattern]

parse :: String -> Input
parse str = map parsePat patterns
  where patterns = L.splitOn "\n\n" str
        parsePat pat = (length cols, length rows,
                        [ (x,y) | (y, row) <- zip [0..] rows
                                , (x, '#') <- zip [0..] row ])
          where rows@(cols : _) = lines pat

compact (nx, ny, rocks) = ( [ U.generate nx (\x -> isRock (x,y)) | y <- [0..ny-1]]
                          , [ U.generate ny (\y -> isRock (x,y)) | x <- [0..nx-1]])
  where
    isRock pos = B.Bit $ pos `elem` rocks

splits xs = [ first reverse $ splitAt i xs | i <- [1 .. length xs - 1]]

mirror xs = fmap (length . fst) $ L.find equal $ splits xs
  where equal (x,y) = and $ zipWith (==) x y

summarize f pat = r + c
  where (rowwise, colwise) = bimap f f $ compact pat
        r = 100 * fromMaybe 0 rowwise
        c = fromMaybe 0 colwise

part1 :: Input -> Int
part1 input = sum $ map (summarize mirror) input
answer1 = part1 <$> input

smudge xs = fmap (length . fst) $ L.find ((== 1) . diffs) $ splits xs
  where diffs (x,y) = sum $ map B.countBits $ zipWith (B.zipBits xor) x y

part2 :: Input -> Int
part2 input = sum $ map (summarize smudge) input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
