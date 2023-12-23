{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Hashable
import qualified Data.HashSet as Set

test =  parse [ "#.#####################"
              , "#.......#########...###"
              , "#######.#########.#.###"
              , "###.....#.>.>.###.#.###"
              , "###v#####.#v#.###.#.###"
              , "###.>...#.#.#.....#...#"
              , "###v###.#.#.#########.#"
              , "###...#.#.#.......#...#"
              , "#####.#.#.#######.#.###"
              , "#.....#.#.#.......#...#"
              , "#.#####.#.#.#########v#"
              , "#.#...#...#...###...>.#"
              , "#.#.#v#######v###.###v#"
              , "#...#.>.#...>.>.#.###.#"
              , "#####v#.#.###v#.#.###.#"
              , "#.....#...#...#.#.#...#"
              , "#.#########.###.#.#.###"
              , "#...###...#...#...#.###"
              , "###.###.#.###v#####v###"
              , "#...#...#.#.>.>.#.>.###"
              , "#.###.###.#.###.#.#v###"
              , "#.....###...###...#...#"
              , "#####################.#"
              ]
input = parse . lines <$> readFile "input.txt"

type Pos = (Int, Int)
data Trail = Path | SlopeU | SlopeD | SlopeL | SlopeR
  deriving (Eq, Show)
type Input = Map Pos Trail

parse :: [String] -> Input
parse rows@(cols : _) = hikingMap
  where hikingMap = Map.fromList [ ((x,y), tr) | (y, row) <- zip [0..] rows
                                               , (x, c) <- zip [0..] row
                                               , tr <- parseTrail c]
        parseTrail = \case
          '.' -> pure Path ; '^' -> pure SlopeU ; 'v' -> pure SlopeD ; '>' -> pure SlopeR;
          '<' -> pure SlopeL; _ -> []

allPaths start next found = dfs Set.empty [[]] start
  where
    dfs visited acc curr
      | found curr = acc
      | curr `Set.member` visited = []
      | otherwise = concatMap (dfs (Set.insert curr visited) (add curr acc)) $ next curr

    add curr = map (curr :)

neighbours (x,y) = \case
  Path   -> [(x-1, y), (x+1,y), (x,y-1), (x,y+1)]
  SlopeU -> [(x, y-1)]
  SlopeD -> [(x, y+1)]
  SlopeL -> [(x-1, y)]
  SlopeR -> [(x+1, y)]

part1 :: Input -> Int
part1 hikingMap = maximum $ map length paths
  where
    paths = allPaths start next found
    (start, Path) = Map.findMin hikingMap
    (end, Path) = Map.findMax hikingMap
    next p = maybe [] (filter (`Map.member` hikingMap) . neighbours p) $ Map.lookup p hikingMap
    found = (end ==)

answer1 = part1 <$> input


neighbours2 (x,y) = \case
  Path   -> [(x-1, y), (x+1,y), (x,y-1), (x,y+1)]
  SlopeU -> [(x, y-1), (x, y+1)]
  SlopeD -> [(x, y+1), (x, y-1)]
  SlopeL -> [(x-1, y), (x+1, y)]
  SlopeR -> [(x+1, y), (x-1, y)]


simplifyGraph :: Pos -> (Pos -> [Pos]) -> (Pos -> Bool) -> Map Pos [(Pos, Int)]
simplifyGraph start next found  = loop [start] Map.empty
  where
    loop [] seen = seen
    loop (curr : rest) seen
      | Map.member curr seen = loop rest seen
      | otherwise = loop (map fst nodes ++ rest) (Map.insert curr nodes seen)
      where nodes = [ n | c <- next curr, n <- straight c curr 1]

    straight from to sofar =
      case filter (to /=) $ next from of
        [corridor] | not $ found corridor -> straight corridor from (sofar + 1)
        _ -> [(from, sofar)]

longestPath :: Pos -> (Pos -> [(Pos, Int)]) -> (Pos -> Bool) -> Int
longestPath start next found = dfs Set.empty 0 (start, 0)
  where
    dfs visited acc (curr, dist)
      | found curr = dist + acc
      | curr `Set.member` visited = -1
      | otherwise = maximum $ map (dfs (Set.insert curr visited) (dist + acc)) $ next curr

part2 :: Input -> Int
part2 hikingMap = longestPath start simplerNext found
  where
    (start, Path) = Map.findMin hikingMap
    (end, Path) = Map.findMax hikingMap
    next p = maybe [] (filter (`Map.member` hikingMap) . neighbours2 p) $ Map.lookup p hikingMap
    found = (end ==)

    simpler = simplifyGraph start next found
    simplerNext n = Map.findWithDefault [] n simpler

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
