{-# LANGUAGE LambdaCase, ViewPatterns,Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L

import qualified Data.Array as A
import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed ((!))
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

test =  parse [ "2413432311323"
              , "3215453535623"
              , "3255245654254"
              , "3446585845452"
              , "4546657867536"
              , "1438598798454"
              , "4457876987766"
              , "3637877979653"
              , "4654967986887"
              , "4564679986453"
              , "1224686865563"
              , "2546548887735"
              , "4322674655533"
              ]
input = parse . lines <$> readFile "input.txt"

type Grid = U.Array (Int, Int) Int
type Input = Grid

parse :: [String] -> Grid
parse rows@(cols : _) = U.array bounds nums
  where nums = [ ((x,y), C.digitToInt d) | (y, row) <- zip [0..] rows
                                         , (x, d) <- zip [0..] row]
        bounds = ((0,0), (length cols - 1, length rows - 1))

showGrid show grid = do
  forM_ [miny .. maxy] $ \y -> do
    forM_ [minx .. maxx] $ \x -> do
       putStr $ show $ grid ! (x,y)
    putStrLn ""
  where ((minx, miny), (maxx, maxy)) = A.bounds grid


type Direction = (Int, Int)
directions@[north, south, east, west] = [ (0,-1), (0,1), (1,0), (-1,0) ]

(a, b) |+| (c, d) = (a+c, b+d)
s |*| (a, b) = (s*a, s*b)
opposite (a,b) = (-a, -b)

showDir d = fromMaybe '?' $  L.lookup d $ zip directions ['^', 'v', '>', '<']

-- | Which direction are we going, and how many steps have we already gone in that direction
type Heading = (Direction, Int)

possible :: Heading -> [Heading]
possible (dir, already) = [ (d, taken) | d <- directions, d /= opposite dir
                                       , let taken = if d == dir then already+1 else 1
                                       , taken <= 3 ]

neighbours grid possible (from, heading) = [ ((to, h), c) | h@(d,_) <- possible heading,
                                                            let to = from |+| d,
                                                            U.bounds grid `U.inRange` to,
                                                            let c = grid ! to]

neighboursAlt grid steps (from, missing) = [ ((to, mis), cost)
                                           | d <- missing,
                                             s <- steps,
                                             let to = from |+| (s |*| d),
                                             U.bounds grid `U.inRange` to,
                                             let range = L.delete from $ U.range (min from to, max from to)
                                                 cost = sum $ map (grid !) range
                                                 mis = turns d]
  where
    turns = \case (0, _) -> [east, west] ; (_, 0) -> [north, south]


dijkstra :: (Num cost, Ord cost, Ord state, H.Hashable state)
         => (state -> [(state, cost)])
         -> (state -> Bool)
         -> state
         -> Maybe cost
dijkstra next found initial = loop initPathCost startFrontier
  where
    x `less` may = maybe True (x <) may
    m !? state = Map.lookup state m

    update n c queue = snd $ Q.alter upsert n queue
      where
        upsert Nothing = ((), Just(c, ()))
        upsert (Just(c', _)) = ((), Just(min c c', ()))

    startFrontier = Q.singleton initial 0 ()
    initPathCost = Map.empty

    loop pathCost (Q.minView -> Nothing) = Nothing
    loop pathCost (Q.minView -> Just(s, c, _, frontier))
      | found s = Just c
      | otherwise = loop pathCost' frontier'
      where
        relevant = [ (n, cc) | (n, sc) <- next s,
                               let cc = c + sc,
                               cc `less` (pathCost !? n) ]
        (frontier', pathCost') = L.foldr updateBoth (frontier, pathCost) relevant
        updateBoth (n, cc) (front, pathC) = (update n cc front, Map.insert n cc pathC)


coolestPath grid start goal = fromMaybe (error "dijkstra couldn't find a path") $
                              dijkstra next found initial
  where next = neighbours grid possible
        found (p, _) = p == goal
        initial = (start, ((0,0),0))


coolestPathAlt grid start goal = fromMaybe (error "dijkstra couldn't find a path") $
                                 dijkstra next found initial
  where next = neighboursAlt grid [1..3]
        found (p, _) = p == goal
        initial = (start, [east, south])


pathToArray grid path = A.accumArray conflict "." bounds $ map (\(p,(d,_)) -> (p, [showDir d])) path
  where bounds = U.bounds grid
        conflict "." s = s
        conflict _ _ = "X"

part1 :: Input -> Int
part1 grid = res
  where (start, goal) = A.bounds grid
        res = coolestPathAlt grid start goal
answer1 = part1 <$> input

possible2 :: Heading -> [Heading]
possible2 (dir, already) = [ (d, taken) | d <- directions, d /= opposite dir
                                        , already > 3 || d == dir || already == 0
                                        , let taken = if d == dir then already+1 else 1
                                        , taken <= 10 ]

ultraCoolestPath grid start goal = fromMaybe (error "dijkstra couldn't find a path") $
                                   dijkstra next found initial
  where next = neighbours grid possible2
        found (p, _) = p == goal
        initial = (start, ((0,0),0))

ultraCoolestPathAlt grid start goal = fromMaybe (error "dijkstra couldn't find a path") $
                                      dijkstra next found initial
  where next = neighboursAlt grid [4..10]
        found (p, _) = p == goal
        initial = (start, [east, south])

part2 :: Input -> Int
part2 grid = res
  where (start, goal) = A.bounds grid
        res = ultraCoolestPathAlt grid start goal
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
