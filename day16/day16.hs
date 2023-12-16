{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.List as L

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import qualified Data.HashSet as Set

import Control.Monad.Par (runPar, parMap)

type Map = HashMap

test =  parse [ ".|...\\...."
              , "|.-.\\....."
              , ".....|-..."
              , "........|."
              , ".........."
              , ".........\\"
              , "..../.\\\\.."
              , ".-.-/..|.."
              , ".|....-|.\\"
              , "..//.|...."
              ]
input = parse . lines <$> readFile "input.txt"

data Mirror = Hori | Vert | Forw | Back
  deriving (Eq, Show, Ord)

type Input = (Int, Int, Map (Int, Int) (Beam -> [Beam]))

parse :: [String] -> Input
parse rows@(cols: _) = (length cols, length rows,
                        Map.fromList[ ((x,y), reflect $ parseMirror m)
                                    | (y, row) <- zip [0..] rows
                                    , (x, m) <- zip [0..] row
                                    , m `elem` "|-/\\"])
  where parseMirror = \case '-' -> Hori ; '|' -> Vert; '/' -> Forw ; _ -> Back

data Direction = North | South | East | West
  deriving (Eq, Show, Ord, Enum)

instance Hashable Direction where
  hashWithSalt = hashUsing fromEnum
  hash = fromEnum

type Beam = (Int, Int, Direction)

reflect :: Mirror -> Beam -> [Beam]
reflect mirror =
  case mirror of
    Hori -> \case (x,y, West)  -> [(x-1, y, West)]
                  (x,y, East)  -> [(x+1, y, East)]
                  (x,y, _)     -> [(x-1, y, West), (x+1, y, East)]
    Vert -> \case (x,y, North) -> [(x, y-1, North)]
                  (x,y, South) -> [(x, y+1, South)]
                  (x,y, _)     -> [(x, y-1, North), (x, y+1, South)]
    Forw -> \case (x,y, North) -> [(x+1, y, East)]
                  (x,y, South) -> [(x-1, y, West)]
                  (x,y, West)  -> [(x, y+1, South)]
                  (x,y, East)  -> [(x, y-1, North)]
    Back -> \case (x,y, North) -> [(x-1, y, West)]
                  (x,y, South) -> [(x+1, y, East)]
                  (x,y, West)  -> [(x, y-1, North)]
                  (x,y, East)  -> [(x, y+1, South)]

continue = \case (x,y, North) -> [(x, y-1, North)]
                 (x,y, South) -> [(x, y+1, South)]
                 (x,y, West)  -> [(x-1, y, West)]
                 (x,y, East)  -> [(x+1, y, East)]

unionsMap f = Set.fromList . concatMap f . Set.toList

step (nr, nc, mirrors) (beams, energized) = (beams', Set.union beams' energized)
  where
    beams' = unionsMap next beams
    next b@(x,y,_) = filter (\b -> inBounds b && not (Set.member b energized)) bs
      where bs = Map.findWithDefault continue (x,y) mirrors b
            inBounds (x,y,_) = 0 <= x && x < nc && 0 <= y && y < nr

energized input start = Set.size $ Set.map (\(x,y,_) -> (x,y)) energized
  where steps = iterate (step input) (Set.singleton start, Set.empty)
        energized = maybe Set.empty snd $ L.find (Set.null . fst) steps

part1 :: Input -> Int
part1 input = energized input (-1, 0, East)
answer1 = part1 <$> input

startPositions nr nc = [ (x,y,dir) | (x, dir) <- [(-1, East), (nc, West)], y <- [0 .. nr-1]] ++
                       [ (x,y,dir) | (y, dir) <- [(-1, South), (nr, North)], x <- [0 .. nc-1]]

part2 :: Input -> Int
part2 input@(nr, nc, _) = maximum potential
  where potential = runPar $ parMap (energized input) $ startPositions nr nc
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
