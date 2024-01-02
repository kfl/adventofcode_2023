{-# LANGUAGE LambdaCase, OverloadedLists #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Data.Bifunctor (first, second)

import Control.Monad.Writer.CPS (execWriter, tell)
import Control.Monad.Par.Scheds.Sparks qualified as CM

import Data.Semigroup

import qualified Data.Bit as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G

import Data.Hashable
import qualified Data.HashSet as Set

import Text.Printf
import System.Process (callCommand)



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

simplifyGraph :: Pos -> (Pos -> [Pos]) -> (V.Vector (U.Vector (Int, Int)), Pos -> Int)
simplifyGraph start next = (vecmap, intern)
  where
    loop seen [] = seen
    loop seen ((curr, _) : rest)
      | Map.member curr seen = loop seen rest
      | otherwise = loop (Map.insert curr nodes seen) (nodes ++ rest)
      where nodes = [ n | c <- next curr, n <- straight curr c 1]

    straight from to sofar =
      case filter (from /=) $ next to of
        [corridor] -> straight to corridor (sofar + 1)
        _ -> [(to, sofar)]

    simpler = loop Map.empty [(start, 0)]

    internMap = Map.fromList $ zip (Map.keys simpler) [0..]
    intern x = internMap ! x
    intmap = Map.fromList [ (intern n, U.fromList $ map (first intern) cs) |
                            (n, cs) <- Map.assocs simpler ]
    vecmap = V.generate (Map.size intmap) (\i -> Map.findWithDefault [] i intmap)

longestPath :: V.Vector (U.Vector (Int, Int)) -> Int -> Int -> Int
longestPath graph start end = getMax $ execWriter (dfs empty 0 (start, 0))
  where
    empty = U.replicate (V.length graph) $ B.Bit False
    member x bs = B.unBit $ bs U.! x
    set x bs = U.modify (\v -> UM.write v x $ B.Bit True) bs
    next i = graph V.! i

    dfs !visited !pathlen (curr, dist)
      | curr == end = tell $ Max $ dist+pathlen
      | otherwise   = U.mapM_ (dfs (set curr visited) (dist+pathlen)) $
                      U.filter (\(n,_) -> not $ n `member` visited) $ next curr


longestPathP :: V.Vector (U.Vector (Int, Int)) -> Int -> Int -> Int
longestPathP graph start end = getMax $ CM.runPar (parallel 0 empty 0 (start, 0))
  where
    empty = U.replicate (V.length graph) $ B.Bit False
    member x bs = B.unBit $ bs U.! x
    set x bs = U.modify (\v -> UM.write v x $ B.Bit True) bs
    next i = graph V.! i

    toVisit n visited = U.convert $ U.filter (\(n, _) -> not $ n `member` visited) $ next n

    mconcatV v = G.foldl' mappend mempty v

    maxDepth = 20
    parMapM f xs = V.mapM (CM.spawn . f) xs >>= V.mapM CM.get

    parallel depth
      | depth >= maxDepth = sequential
      | otherwise = common (parallel (depth + 1)) parMapM

    sequential = common sequential U.mapM

    common recurse mapF visited pathlen (curr, dist)
      | curr == end = pure $ Max $ dist + pathlen
      | otherwise   = do let nextNodes = toVisit curr visited
                         mconcatV <$> mapF (recurse (set curr visited) (dist + pathlen)) nextNodes


showSimplerGraph name hikingMap = do
  let (graph, start, end) = fromHikingMap hikingMap
      trans = [ printf "  %d -> %d [label=\"%d\"];" n1 n2 c | (n1, cs) <- V.toList $ V.indexed graph, (n2, c) <- U.toList cs]
      dotfile = name ++ ".dot"
      pdffile = name ++ ".pdf"
      pre = "digraph connections {"
  writeFile dotfile $ unlines $ pre : trans ++ ["}"]
  callCommand $ "neato -Tpdf "++dotfile ++" -o "++pdffile
  callCommand $ "open -a Preview "++pdffile

fromHikingMap hikingMap = (simpler, intern start, intern end)
  where
    (start, Path) = Map.findMin hikingMap
    (end, Path) = Map.findMax hikingMap
    next p = maybe [] (filter (`Map.member` hikingMap) . neighbours2 p) $ Map.lookup p hikingMap

    (simpler, intern) = simplifyGraph start next

part2 :: Input -> Int
part2 hikingMap = longestPathP simpler start end
  where
    (simpler, start, end) = fromHikingMap hikingMap

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
