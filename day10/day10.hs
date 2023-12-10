{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import qualified Data.Bifoldable as Bi
import Data.Bifunctor (bimap)


import qualified Data.List.Split as L
import System.Process (callCommand)

test1 =  parse [ "-L|F7"
               , "7S-7|"
               , "L|7||"
               , "-L-J|"
               , "L|-JF"
               ]
test2 = parse [ "7-F7-"
              , ".FJ|7"
              , "SJLL7"
              , "|F--J"
              , "LJ.LJ"
              ]
input = parse . lines <$> readFile "input.txt"

type Tile = (Int, Int)
type Connections = (Tile, Tile)
type Tiles = Map Tile Connections

type Input = (Tile, Tiles)

north (r,c) = (r-1, c)
south (r,c) = (r+1, c)
east (r,c) = (r, c+1)
west (r,c) = (r, c-1)

connections t = \case
  '|' -> pure (north t, south t)
  '-' -> pure (east t, west t)
  'L' -> pure (north t, east t)
  'J' -> pure (north t, west t)
  '7' -> pure (south t, west t)
  'F' -> pure (south t, east t)
  _   -> Nothing

parse :: [String] -> Input
parse strs = (fromJust s, m)
  where
    (_, s, m) = L.foldl' row (0, Nothing, Map.empty) strs
    row (i, s, m) line = (i+1, s', m')
      where (_, s', m') = L.foldl' (col i) (0, s, m) line
    col i (j, s, m) c | c == 'S' = (j+1, pure (i,j), m)
                      | otherwise = (j+1, s,
                                     maybe m (insertIn (i,j) m) $ connections (i,j) c)
    insertIn k m v = Map.insert k v m

startConnections start ts = (c1, c2)
  where [c1, c2] = [ k | (k, cs) <- Map.assocs ts, start `Bi.bielem` cs]

path tiles (prev : past) curr = maybe history (path tiles history) next
  where history = curr : prev : past
        next = Map.lookup curr tiles >>= Bi.bifind (/= prev)

mainLoop (start, tiles) = p
  where p = path tiles [start] c1
        (c1, _) = startConnections start tiles

part1 :: Input -> Int
part1 input = length (mainLoop input) `div` 2
answer1 = part1 <$> input

test3 = parse [ ".........."
              , ".S------7."
              , ".|F----7|."
              , ".||OOOO||."
              , ".||OOOO||."
              , ".|L-7F-J|."
              , ".|II||II|."
              , ".L--JL--J."
              , ".........."
              ]

larger1 = parse [ ".F----7F7F7F7F-7...."
                , ".|F--7||||||||FJ...."
                , ".||.FJ||||||||L7...."
                , "FJL7L7LJLJ||LJ.L-7.."
                , "L--J.L7...LJS7F-7L7."
                , "....F-J..F7FJ|L7L7L7"
                , "....L7.F7||L7|.L7L7|"
                , ".....|FJLJ|FJ|F7|.LJ"
                , "....FJL-7.||.||||..."
                , "....L---J.LJ.LJLJ..."
                ]

larger2 = parse [ "FF7FSF7F7F7F7F7F---7"
                , "L|LJ||||||||||||F--J"
                , "FL-7LJLJ||||||LJL-77"
                , "F--JF--7||LJLJ7F7FJ-"
                , "L---JF-JLJ.||-FJLJJ7"
                , "|F|F-JF---7F7-L7L|7|"
                , "|FFJF7L7F-JF7|JL---7"
                , "7-L-JL7||F7|L7F-7F7|"
                , "L.L7LFJ|||||FJL7||LJ"
                , "L7JLJL-JLJLJL--JLJ.L"
                ]

visualise file start@(sr,sc) nodes edges = do
  let name t = show $ show t
      node t@(r,c) = [name t, "[ pos = \"", show $ 100 *c, ",", show (-100 * r), "\"];\n"]
      startNode = concat[name start,
                         "[ pos = \"", show $ 100 *sc, ",", show (-100 * sr), "\", color=green];\n"]
      ns = startNode : map (concat . node) nodes
      es = map ((++";\n") . L.intercalate " -- ") $ L.divvy 2 1 $ map name edges
      dotfile = file ++ ".dot"
      pdffile = file ++ ".pdf"
  writeFile dotfile $ concat $ "graph grid { node [shape=point];\n" : ns ++ es ++ ["}\n"]
  callCommand $ "neato -Tpdf -n "++dotfile++" -o "++pdffile
  callCommand $ "open -a Preview " ++ pdffile

showTiles file (start, tiles) = do
  let (c1, _) = startConnections start tiles
      edges = path tiles [start] c1
      nodes = Map.keys tiles
  visualise file start nodes edges

showPath file p@(start : _) = do
  visualise file start p p

part2 :: Input -> Int
part2 (start, tiles) = sum scanRowCount
  where sc@(c1, _) = startConnections start tiles
        p = path tiles [start] c1
        tiles' = Map.insert start sc tiles
        pset = Set.fromList p
        (maxr, maxc) = bimap maximum maximum $ unzip $ Map.keys tiles
        onPath t = t `Set.member` pset

        winding t@(r, _) (goingUp, outside) =
          case tiles' ! t of
            ((_, c1), (_, c2)) | c1 == c2 -> (Nothing, not outside)
            ((r1, _), (r2, _)) | r1 == r2 -> (goingUp, outside)
            ((r1, _), (r2, _)) ->
               case goingUp of
                 Nothing -> (Just $ r1 > r || r2 > r, outside)
                 Just upwards -> (Nothing, (upwards /= (r1 > r || r2 > r)) /= outside)

        scanPoint t (count, acc@(_, outside))
          | onPath t = (count, winding t acc)
          | outside   = (count, acc)
          | otherwise = (count + 1, acc)
        scanColCount r = count
          where (count, _) = foldr (\c acc -> scanPoint (r,c) acc) (0, (Nothing, True)) [0..maxc]
        scanRowCount = map scanColCount [0..maxr]

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
