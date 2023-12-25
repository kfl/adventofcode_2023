{-# LANGUAGE LambdaCase, Strict, OverloadedLists, ViewPatterns #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Bifunctor (bimap,first,second)

import Data.Graph qualified as G

import Control.Monad.Trans.State.Strict (execState)

import Text.Printf
import System.Process (callCommand)
import Text.Pretty.Simple qualified as P


test = parse [ "jqt: rhn xhk nvd"
             , "rsh: frs pzl lsr"
             , "xhk: hfx"
             , "cmg: qnr nvd lhk bvb"
             , "rhn: xhk bvb hfx"
             , "bvb: xhk hfx"
             , "pzl: lsr hfx nvd"
             , "qnr: nvd"
             , "ntq: jqt hfx bvb xhk"
             , "nvd: lhk"
             , "lsr: lhk"
             , "rzs: qnr cmg lsr rsh"
             , "frs: qnr lhk lsr"
             ]
input = parse . lines <$> readFile "input.txt"

type Name = String
type Connections = Map Name (Set Name)
type Input = Connections

parse :: [String] -> Input
parse strs = Map.fromList $ map parseLine strs
  where parseLine str = (name, Set.fromList connected)
          where (name, _ : comps) = break (== ':') str
                connected = words comps

bothWays :: Connections -> Connections
bothWays conn = Map.foldlWithKey' backedges initial conn
  where backedges acc n connections = L.foldl' (edge n) acc connections
        edge n acc c = Map.insertWith Set.union c [n] acc
        initial = conn

edgeSet :: Connections -> Set (Set Name)
edgeSet conns = Map.foldlWithKey' edges Set.empty conns
  where edges acc n connections = L.foldl' (edge n) acc connections
        edge n acc c = Set.insert ([n, c]) acc

fromEdgeSet :: Set (Set Name) -> Connections
fromEdgeSet edges = Map.fromListWith Set.union cs
  where ~cs = [ kd | edge <- Set.toList edges
                   , let [n1, n2] = Set.toList edge, kd <- [(n1, [n2]), (n2, [n1])]]

invariant conn = bothWays conn == (fromEdgeSet $ edgeSet conn)

showConnections name conns = do
  let edges = edgeSet conns
      ~trans = [ printf "  %s -- %s" n1 n2 | [n1, n2] <-  map Set.toList $ Set.toList edges]
      dotfile = name ++ ".dot"
      pdffile = name ++ ".pdf"
      pre = "graph connections {"
  writeFile dotfile $ unlines $ pre : trans ++ ["}"]
  callCommand $ "neato -Tpdf "++dotfile ++" -o "++pdffile
  callCommand $ "open -a Preview "++pdffile


-- Cheating by manually reading out of the visualisation which edges to remove
cheese _ = [["ldk", "bkm"], ["rsm","bvc"], ["zmq","pgh"]]

disconnect conns bridges = (group1, Map.keysSet disconnected Set.\\ group1)
  where edges = edgeSet conns
        bs = bridges conns
        disconnected = fromEdgeSet $ edges Set.\\ bs

        collect strongly (Set.minView -> Nothing) = strongly
        collect strongly (Set.minView -> Just(n, rest))
          | Set.member n strongly = collect strongly rest
          | otherwise = collect (Set.insert n strongly) $ Set.union rest $ disconnected ! n

        (someElem, _) = Map.findMin disconnected
        group1 = collect [] [someElem]

bridges conns = map ((fmap name)) (G.scc graph)
  where
    edgeList = [ (name, name, Set.toList cs) | (name, cs) <- Map.assocs $ bothWays conns ]
    (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges edgeList
    name v = case nodeFromVertex v of (n, _, _) -> n

pPrint x = P.pPrintOpt P.CheckColorTty smallIndent x
  where smallIndent = P.defaultOutputOptionsNoColor { P.outputOptionsIndentAmount = 2
                                                    , P.outputOptionsCompactParens = True}

part1 :: Input -> Int
part1 input = Set.size group1 * Set.size group2
  where (group1, group2) = disconnect input cheese
answer1 = part1 <$> input

part2 input = "Merry Xmas 2023"
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
