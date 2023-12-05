{-# LANGUAGE LambdaCase, NamedFieldPuns, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Function ( (&) )
-- import qualified Data.Map.Strict as Map
-- import Data.Map.Strict (Map)

test =  parse <$> readFile "test.txt"
input = parse <$> readFile "input.txt"

data Rule = Rule { dst :: Int, src :: Int, range :: Int }
  deriving (Eq, Show, Ord)
type Mapping = [Rule]
type Seeds = [Int]
type Input = (Seeds, [Mapping])

parse :: String -> Input
parse str = (seeds, maps)
  where seedss : mapss = L.splitOn "\n\n" str
        seeds = seedss & L.splitOn ":" & (!! 1) & words & map read
        rule line = Rule{dst, src, range}
          where [dst, src, range] = map read $ words line
        mapping (_ : lines) = map rule lines
        maps = map mapping $ map lines mapss

match key (Rule{dst, src, range}) = src <= key && key < src + range

convert key mapping = maybe key byRule $ L.find (match key) mapping
  where byRule (Rule{dst, src, range}) = dst + (key - src)

full mappings key = L.foldl' convert key mappings

part1 :: Input -> Int
part1 (seeds, mappings) = minimum locs
  where locs = map (full mappings) seeds
answer1 = part1 <$> input

pairUp (x:y:rest) = (x,y) : pairUp rest
pairUp _ = []

seedCount seeds = sum $ map snd $ pairUp seeds
allSeeds seeds = [ s | (start, rng) <- pairUp seeds, s <- [start .. start+rng-1]]

infixr 4 `whenPos`
x@(_, n) `whenPos` xs | n > 0 = x : xs
_ `whenPos` xs = xs

ruleRange (Rule{dst, src, range}) (key, rng) =
  ((dst+off, overlap), if overlap > 0 then (key, before) `whenPos` (y, after) `whenPos` []
                       else [(key,rng)])
  where
    a = key
    b = key + rng
    x = src
    y = src + range
    off = if key > src then key - src else 0
    overlap = max 0 (min b y - max a x)
    before = if overlap > 0 && a < x then x - a else 0
    after = if overlap > 0 && y < b then rng - (before + overlap) else 0

convertRange seedRanges mapping = addGood missing converted
  where
    addGood = foldr whenPos
    accumRule (converted, seedRanges) rule =
          let (convs, missing) = unzip $ map (ruleRange rule) seedRanges
          in (addGood converted convs, concat missing)
    (converted, missing) = L.foldl' accumRule ([], seedRanges) mapping

fullRange mappings seedRanges = L.foldl' convertRange seedRanges mappings

part2 :: Input -> Int
part2 (seeds, mappings) = fst $ minimum locs
  where locs = fullRange mappings $ pairUp seeds
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
