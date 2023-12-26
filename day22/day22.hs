{-# LANGUAGE ViewPatterns, Strict #-}
module Main where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set

import Control.Monad.State.Strict (evalState, get, put)

import Text.Printf
import System.Process (callCommand)

test =  map parse [ "1,0,1~1,2,1"
                  , "0,0,2~2,0,2"
                  , "0,2,3~2,2,3"
                  , "0,0,4~0,2,4"
                  , "2,0,5~2,2,5"
                  , "0,1,6~2,1,6"
                  , "1,1,8~1,1,9"
                  -- , "1,1,11~1,1,12"  -- Extra brick for debugging
                  -- , "1,0,10~1,2,10"  -- Extra brick for debugging
                  -- , "1,0,18~1,0,19"  -- Extra brick for debugging
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
    show (C x y z w h d) = printf "(C %d %d %d %d %d %d)" x y z w h d


brickToCuboid :: Brick -> Cuboid
brickToCuboid ((x1,y1,z1), (x2,y2,z2)) = C x1 y1 z1 (x2-x1+1) (y2-y1+1) (z2-z1+1)

showCuboids cuboids = do
  let lines = [printf "%d, %d, %d, %d, %d, %d" x y z w h d | C{x, y, z, w, h, d} <- cuboids]
  writeFile "cuboids.txt" $ unlines lines
  callCommand "./viewcuboids.py"
showBricks bricks = showCuboids $ map brickToCuboid bricks


rectOverlap (C{x=x1,y=y1,w=w1,h=h1}) (C{x=x2,y=y2,w=w2,h=h2}) =
  x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1

oneFall (zmap, supporters) c = (insert (z+d) [c'] zmap, insert c' supps supporters)
  where
    falling (Map.maxViewWithKey -> Nothing) = (c{z = 1}, [])
    falling (Map.maxViewWithKey -> Just((zlevel, potentials), rest)) =
      case filter (rectOverlap c) potentials of
        [] -> falling rest
        supporting -> (c{z = zlevel}, supporting)
    (c'@C{z, d}, supps) = falling zmap
    insert k = Map.insertWith (++) k

downFall cuboids = L.foldl' oneFall (Map.empty, Map.empty) $ L.sortOn z cuboids

fallen bricks = cuboids
  where
    (zmap, supporters) = downFall $ map brickToCuboid bricks
    cuboids = concat $ Map.elems zmap


cuboidsOverlap (C x1 y1 z1 w1 h1 d1) (C x2 y2 z2 w2 h2 d2) =
    x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1
    && z1 < z2 + d2 && z2 < z1 + d1

overlappingCuboids cuboids = [(c1, c2) | (c1:cs) <- L.tails cuboids, c2 <- cs
                                       , cuboidsOverlap c1 c2]

part1 :: Input -> Int
part1 input = length input - Set.size essentials
  where
    (zmap, supporters) = downFall $ map brickToCuboid input
    essentials = Set.fromList $ concat $ filter ((== 1) . length) $ Map.elems supporters
answer1 = part1 <$> input


inverse mapping = Map.fromListWith (++) [ (s, [c]) | (c, supps) <- Map.assocs mapping, s <- supps ]

chain c = do
  (supporting, supporters) <- get
  if null (supporters Map.! c)
    then do acc <- remove c
            return $ 1 + acc
    else return 0

remove c = do
  (supporting, supporters) <- get
  let supp = Map.findWithDefault [] c supporting
      supporters' = L.foldr (Map.adjust (L.delete c)) supporters supp
  put (supporting, supporters')
  sum <$> mapM chain supp

part2 input = sum reactions
  where
    (zmap, supporters) = downFall $ map brickToCuboid input
    essentials = Set.fromList $ concat $ filter ((== 1) . length) $ Map.elems supporters
    supporting = inverse supporters
    reactions = [ evalState (remove c) (supporting, supporters) | c <- Set.toList essentials]

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
