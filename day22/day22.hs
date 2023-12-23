{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)


import Data.Function (on)

import Text.Printf
import System.Process (callCommand)


test =  map parse [ "1,0,1~1,2,1"
                  , "0,0,2~2,0,2"
                  , "0,2,3~2,2,3"
                  , "0,0,4~0,2,4"
                  , "2,0,5~2,2,5"
                  , "0,1,6~2,1,6"
                  , "1,1,8~1,1,9"
                  , "1,1,11~1,1,12"  -- Extra brick for debugging
                  , "1,0,15~1,0,16"  -- Extra brick for debugging
                  ]
input = map parse . lines <$> readFile "input.txt"

type Coor = (Int, Int, Int)
type Brick = (Coor, Coor)

type Input = [Brick]

parse :: String -> Brick
parse str = (coor start, coor end)
  where (start, _: end) = break (== '~') str
        coor s = read $ "("++s++")"

data Cuboid = C { x, y, z, w, h, d :: Int }
  deriving (Eq, Ord)

instance Show Cuboid where
    show (C x y z w h d) = printf "(C %d %d %d %d %d %d)\n" x y z w h d


brickToCuboid :: Brick -> Cuboid
brickToCuboid ((x1,y1,z1), (x2,y2,z2)) = C x1 y1 z1 (x2-x1+1) (y2-y1+1) (z2-z1+1)

showCuboids cuboids = do
  let lines = [printf "%d, %d, %d, %d, %d, %d" x y z w h d | C{x, y, z, w, h, d} <- cuboids]
  writeFile "cuboids.txt" $ unlines lines
  callCommand "./viewcuboids.py"
showBricks bricks = showCuboids $ map brickToCuboid bricks

rectOverlap (C{x=x1,y=y1,w=w1,h=h1}) (C{x=x2,y=y2,w=w2,h=h2}) =
  x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1

oneFall placed c = over ++ c' : under
  where (over, under) = break (rectOverlap c) placed
        c' = c{z = case under of [] -> 1; C{z, d} : _ -> z+d}

downFall cuboids = L.foldl' oneFall [] $ L.sortOn z cuboids


--part1 :: Input -> Int
part1 input = length cuboids - Set.size essentials
  where
    cuboids = downFall $ map brickToCuboid input
    zmap = Map.fromListWith (++) [ (top, [c]) | c@C{z, d} <- cuboids, let top = z+d ]
    potentials z = Map.findWithDefault [] z zmap
    supporters = Map.fromListWith (++) [ (c, [p]) | c@C{z} <- cuboids
                                                  , p <- potentials z
                                                  , rectOverlap c p ]
    essentials = Set.fromList $ concat $ filter ((== 1) . length) $ Map.elems supporters
    cset = Set.fromList cuboids
    safe = Set.difference cset essentials
    freeloaders = Set.difference cset $ Set.fromList $ concat $ Map.elems supporters
    -- FIXME: For future debugging this invariant does not hold: length input == Set.size cset
    --        There is properly a bug in downFall

answer1 = part1 <$> input

-- part2 :: Input -> Int
-- part2 input = undefined
-- answer2 = part2 <$> input

main = undefined -- do
  -- inp <- input
  -- print $ part1 inp
  -- print $ part2 inp
