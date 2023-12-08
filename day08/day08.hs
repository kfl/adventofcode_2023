{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import System.Process (callCommand)

type Instruction = Char -- 'L' or 'R'
type Node = String
type Network = Map Node (Node, Node)

test =  parse $ unlines ["LLR"
                        , ""
                        , "AAA = (BBB, BBB)"
                        , "BBB = (AAA, ZZZ)"
                        , "ZZZ = (ZZZ, ZZZ)"
                        ]
input = parse <$> readFile "input.txt"


type Input = ([Instruction], Network)

parse :: String -> Input
parse str = (instrs, Map.fromList net)
  where [instrs, nodes] = L.splitOn "\n\n" str
        net = map (\case [n, lr] -> (n, readPair lr)) $ map (L.splitOn " = ") $ lines nodes
        readPair s = (left, right)
          where [left, right] = words $ filter (`notElem` "(),") s

startNode = "AAA"
endNode = "ZZZ"

step net (node, instr : rest) = (apply instr $ net ! node, rest)
apply = \case 'L' -> fst; 'R' -> snd

part1 :: Input -> Int
part1 (instrs, net) = fromJust $ L.elemIndex endNode run
  where run = map fst $ iterate (step net) (startNode, L.cycle instrs)
answer1 = part1 <$> input

test2 = parse $ unlines [ "LR"
                        , ""
                        , "11A = (11B, XXX)"
                        , "11B = (XXX, 11Z)"
                        , "11Z = (11B, XXX)"
                        , "22A = (22B, XXX)"
                        , "22B = (22C, 22C)"
                        , "22C = (22Z, 22Z)"
                        , "22Z = (22B, 22B)"
                        , "XXX = (XXX, XXX)"
                        ]

part2 :: Input -> Int
part2 (instrs, net) = leastCommonMultiplier
  where
    startNodes = filter ("A" `L.isSuffixOf`) $ Map.keys net
    endNodes = Set.filter ("Z" `L.isSuffixOf`) $ Map.keysSet net
    run startNode = map fst $ iterate (step net) (startNode, L.cycle instrs)
    runs = map run startNodes

    -- Assumption: once you hit an end-node, then you enter a cycle
    cycleLengths = map (fromJust . L.findIndex (`Set.member` endNodes)) runs
    leastCommonMultiplier = foldr1 lcm cycleLengths

answer2 = part2 <$> input

showNetwork = do
  (_, net) <- input
  let trans = map (\(n, (n1, n2)) -> concat[n, " -> ", n1, ";\n",
                                            n, " -> ", n2, ";\n"]) $ Map.assocs net
  writeFile "input.dot" $ concat $ "digraph network { \n" : trans ++ ["}\n"]
  callCommand "neato -Tpdf input.dot -o input.pdf"
  callCommand "open -a Preview input.pdf"

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
