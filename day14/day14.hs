{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Control.Monad (forM_)

test =  parse [ "O....#...."
              , "O.OO#....#"
              , ".....##..."
              , "OO.#O....O"
              , ".O.....O#."
              , "O.#..O.#.#"
              , "..O..#O..O"
              , ".......O.."
              , "#....###.."
              , "#OO..#...."
              ]
input = parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Input = (Int, Int, [((Col, Row), Bool)])

parse :: [String] -> Input
parse rows@(cols: _) = (length cols, length rows,
                        [ ((x,y), c == 'O') | (y, row) <- zip [0..] rows
                                            , (x, c) <- zip [0..] row
                                            , c `elem` "O#"])

type Rockmap a b = Map a (Map b Bool)

toRockMap :: (Int, Int, [((Col, Row), Bool)]) -> Rockmap Row Col
toRockMap (nr, nc, rocks) = L.foldl' insert init rocks
  where init = Map.fromList [ (y, Map.empty) | y <- [0 .. nr - 1]]
        insert m ((x,y), rounded) = Map.adjust (Map.insert x rounded) y m

trav :: Rockmap a b -> [((b, a), Bool)]
trav rcm = [ ((x,y), rounded) | (y, row) <- Map.assocs rcm
                              , (x, rounded) <- Map.assocs row]

tilt trav slide nextInit rocks = Map.map snd tiltedRocks
  where
    n = Map.size rocks
    init = Map.fromList [ (x, (nextInit, Map.empty)) | x <- [0 .. n - 1]]
    alter m ((x,y), rounded) = Map.insert x (next', Map.insert y' rounded xm) m
      where (next, xm) = m ! x
            (next', y') = slide rounded next y
    tiltedRocks = L.foldl' alter init $ trav rocks

slideDown rounded next y = (y'+1, y')
 where y' = if rounded then next else y

tiltNorth :: Rockmap Row Col -> Rockmap Col Row
tiltNorth = tilt trav slideDown 0

northLoad :: Rockmap Col Row -> Int
northLoad rocks = sum [n - y | col <- Map.elems rocks
                             , (y, True) <- Map.assocs col]
  where n = Map.size rocks


part1 :: Input -> Int
part1 input@(nr, _, _) = northLoad $ tiltNorth $ toRockMap input

answer1 = part1 <$> input

tiltWest :: Rockmap Col Row -> Rockmap Row Col
tiltWest = tiltNorth

slideUp rounded next y = (y'-1, y')
 where y' = if rounded then next else y

tiltSouth :: Rockmap Row Col -> Rockmap Col Row
tiltSouth m = tilt (reverse . trav) slideUp (Map.size m - 1) m

tiltEast :: Rockmap Col Row -> Rockmap Row Col
tiltEast = tiltSouth

spinCycle m = tiltEast $ tiltSouth $ tiltWest $ tiltNorth m

findCycle xs = loop Map.empty $ zip [0..] xs
  where
    loop seen ((steps, x) : rest) =
      case Map.lookup x seen of
        Nothing -> loop (Map.insert x steps seen) rest
        Just before -> (before, steps)


showRocks m = do
  let n = Map.size m
  forM_ [0 .. n-1] $ \y -> do
    forM_ [0 .. n-1] $ \x -> do
      putStr $ case Map.lookup x $ m ! y of
                 Just True -> "O"
                 Just _    -> "#"
                 _         -> "."
    putStrLn ""

northLoad2 :: Rockmap Row Col -> Int
northLoad2 rocks = sum  [ n - y | (y, row) <- Map.assocs rocks
                                , True <- Map.elems row]
  where n = Map.size rocks

part2 input = northLoad2 $ spins !! i
  where start = toRockMap input
        spins = iterate spinCycle start
        (before, steps) = findCycle spins
        cycleLength = steps - before
        i = before + (1_000_000_000 - before) `mod` cycleLength

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
