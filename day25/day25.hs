{-# LANGUAGE Strict, OverloadedLists, ViewPatterns #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Bifunctor (bimap,first,second)

import Data.Function (on)
import Data.Ord (Down(..))
import Algorithm.Search qualified as AS
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Control.Monad (forM_)

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
type Connections name = Map name (Set name)
type Input = Connections Name

parse :: [String] -> Input
parse strs = Map.fromList $ map parseLine strs
  where parseLine str = (name, Set.fromList connected)
          where (name, _ : comps) = break (== ':') str
                connected = words comps

bothWays :: Input -> Input
bothWays conn = Map.foldlWithKey' backedges initial conn
  where backedges acc n = L.foldl' (edge n) acc
        edge n acc c = Map.insertWith Set.union c [n] acc
        initial = conn

edgeSet :: (Ord name, Foldable t) => Map name (t name) -> Set (Set name)
edgeSet = Map.foldlWithKey' edges Set.empty
  where edges acc n = L.foldl' (edge n) acc
        edge n acc c = Set.insert [n, c] acc


fromEdgeSet :: Ord k => Set (Set k) -> Connections k
fromEdgeSet edges = Map.fromListWith Set.union cs
  where ~cs = [ kd | edge <- Set.toList edges
                   , let [n1, n2] = Set.toList edge, kd <- [(n1, [n2]), (n2, [n1])]]

invariant conn = bothWays conn == fromEdgeSet (edgeSet conn)

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
        bs = bridges $ fromEdgeSet edges
        disconnected = fromEdgeSet $ edges Set.\\ bs

        collect strongly (Set.minView -> Nothing) = strongly
        collect strongly (Set.minView -> Just(n, rest))
          | Set.member n strongly = collect strongly rest
          | otherwise = collect (Set.insert n strongly) $ Set.union rest $ disconnected ! n

        (someElem, _) = Map.findMin disconnected
        group1 = collect [] [someElem]


pPrint x = P.pPrintOpt P.CheckColorTty smallIndent x
  where smallIndent = P.defaultOutputOptionsNoColor { P.outputOptionsIndentAmount = 2
                                                    , P.outputOptionsCompactParens = True}

allPairs keys = [ (k1, k2) | k1 : ks <- L.tails keys, k2 <- ks]

pairUp (x:y:rest) = (x,y) : pairUp rest
pairUp _ = []

bridges conns' = Set.fromList $ map (Set.map retern . (`Set.elemAt` edges)) $ take 3 topEdges
  where internMap = Map.fromList $ zip (Map.keys conns') [0..]
        intern s = internMap ! s
        reternMap = Map.fromList $ map (\(x,y) -> (y,x)) $ Map.toList internMap
        retern i = reternMap ! i

        conns :: Map Int [(Int, Int)]
        conns = Map.fromList [ (intern n, map ((,1) . intern) $ Set.toList cs) |
                               (n, cs) <- Map.assocs conns']
        edges = edgeSet $ Map.map (map fst) conns

        pairs = zip [0..] $ allPairs $ Map.keys conns
        edgy (n1,n2) = Set.findIndex [n1,n2] edges

        next k = conns ! k

        loop :: U.Vector Int -> U.Vector Int
        loop = U.modify $ \counts -> do
          forM_ pairs $ \(pi, (src, dst)) -> do
            case AS.dijkstraAssoc next (== dst) src of
              Just (_, path) -> forM_ (pairUp path) $ \ e -> do
                UM.modify counts (+1) $ edgy e
        edgeCount = loop $ U.replicate (Set.size edges) 0
        topEdges = map fst $ L.sortOn (Down . snd) $ U.toList $ U.indexed edgeCount



part1 :: Input -> Int
part1Cheese input = Set.size group1 * Set.size group2
  where (group1, group2) = disconnect input cheese
part1 input = Set.size group1 * Set.size group2
  where (group1, group2) = disconnect input bridges
answer1 = part1 <$> input

part2 input = "Merry Xmas 2023"
answer2 = part2 <$> input

main = do
  inp <- input
  print $ "Cheating solution: " ++ show (part1Cheese inp)
  print $ part1 inp
  print $ part2 inp
