{-# LANGUAGE Strict #-}
module Main where

import qualified Data.Char as C
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP

test =  map parse [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                  , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
                  , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
                  , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                  , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
                  ]
input = map parse . lines <$> readFile "input.txt"

data Colour = Red | Green | Blue
  deriving (Eq, Read, Show, Ord, Enum)

type Id = Int
type Grab = [(Colour, Int)]
type Game = (Id, [Grab])
type Input = [Game]

parse :: String -> Game
parse str = res
  where [(res, _)] = readP_to_S (line <* eof) str
        line =  (,) <$> (symbol "Game" *> int <* symbol ":") <*> grabs
        grabs = sepBy grab (symbol ";")
        grab = sepBy (pairing <$> int <*> colour) (symbol ",")
        pairing n c = (c, n)
        colour = choice [symbol "red" >> return Red,
                         symbol "green" >> return Green,
                         symbol "blue" >> return Blue]
        symbol s = string s <* munch C.isSpace
        int = read <$> munch1 C.isDigit <* munch C.isSpace

type Constrains = Grab

standard :: Constrains
standard = [(Red, 12), (Green, 13), (Blue, 14)]

possible :: Constrains -> Grab -> Bool
possible constrains grab = all check constrains
  where check (colour, limit) = maybe True (<= limit) $ lookup colour grab

good :: Constrains -> Game -> Bool
good constrains (_, grabs) = all (possible constrains) grabs

part1 :: Input -> Int
part1 input = sum $ map fst $ filter (good standard) input
answer1 = part1 <$> input

power (_, grabs) = maximum reds * maximum greens * maximum blues
  where reds = mapMaybe (lookup Red) grabs
        greens = mapMaybe (lookup Green) grabs
        blues = mapMaybe (lookup Blue) grabs

part2 input = sum $ map power input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
