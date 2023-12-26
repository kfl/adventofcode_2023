{-# LANGUAGE OverloadedLists #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.IntMap.Strict as IMap

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.STRef as ST
import qualified Control.Monad.ST as ST
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Data.Bifunctor (first, second)

test1 =  map parse [ "broadcaster -> a, b, c"
                   , "%a -> b"
                   , "%b -> c"
                   , "%c -> inv"
                   , "&inv -> a"
                   ]
test2 =  map parse [ "broadcaster -> a"
                   , "%a -> inv, con"
                   , "&inv -> b"
                   , "%b -> con"
                   , "&con -> output"
                   ]

input = map parse . lines <$> readFile "input.txt"

data Transfer = FlipFlop Bool | Conj (Map Int Bool) | Special
  deriving (Eq, Show, Ord)
type Module name = (name, Transfer, [name])

type Input = [Module String]

parse :: String -> Module String
parse str = (name, trans, names)
  where [tname, namess] = L.splitOn " -> " str
        names = L.splitOn ", " namess
        (name, trans) = case tname of
                          '%' : rest -> (rest, FlipFlop False)
                          '&' : rest -> (rest, Conj [])
                          _          -> (tname, Special)

internalise :: [Module String] -> (V.Vector (Transfer, [Int]),
                                   Map String Int,
                                   Map Int (Map Int Bool))
internalise modules = (V.fromList [ complete m | m <- modules],
                       internMap,
                       inputMap)
  where internMap = Map.fromList [ (n, i) | (i, (n, _, _)) <- zip [0..] modules]
        intern s = Map.findWithDefault (-1) s internMap
        inputMap = Map.fromListWith Map.union [ (intern d, Map.singleton (intern n) False)
                                              | (n, t, dsts) <- modules,
                                                d <- dsts ]
        complete (n, t, dsts) = (tr, map intern dsts)
          where tr = case t of
                       Conj _ -> Conj $ inputMap Map.! intern n
                       _ -> t

apply dsts (from, to, pul) tr =
  case tr of
    FlipFlop old | not pul -> (FlipFlop $ not old, [ (to, n, not old) | n <- dsts])
    Conj args -> (Conj args', [ (to, n, not sig) | n <- dsts])
      where args' = Map.insert from pul args
            sig = and $ Map.elems args'
    Special -> (tr, [ (to, n, pul) | n <- dsts])
    _ -> (tr, [])


-- FIXME: the function buttonPress is an abomination, it should just be a pure
-- function the keepPressing below. However I leave it around for
-- historical reasons (for now at least)

buttonPress start (trans, c) = ST.runST $ do
  transm <- V.thaw trans
  counters <- ST.newSTRef c
  let q = Seq.singleton (-1, start, False)
  let loop (p@(from, to, pul) :<| q) = do
        let sel = if pul then first else second
        ST.modifySTRef counters (sel (+1))
        if to /= -1 then do
          (old, dsts) <- VM.read transm to
          let (new, puls) = apply dsts p old
          VM.write transm to (new, dsts)
          loop (q >< Seq.fromList puls)
        else
          loop q
      loop _ = return ()
  loop q
  c' <- ST.readSTRef counters
  trans' <- V.unsafeFreeze transm
  return (trans', c')

part1 :: Input -> Int
part1 input = highs * lows
  where (trans, intern, _) = internalise input
        start = intern Map.! "broadcaster"
        step = buttonPress start
        steps = iterate step (trans, (0,0))
        res@(_, (highs, lows)) = steps !! 1000
answer1 = part1 <$> input


keepPressing start trans = loop trans restartQ
  where
    restartQ = Seq.singleton (-1, start, False)

    loop trans Empty                = loop trans restartQ
    loop trans (p@(_, to, _) :<| q) = p : loop trans' q'
      where
        (trans', q') = case IMap.lookup to trans of
                         Nothing -> (trans, q)
                         Just (old, dsts) ->
                           let (new, puls) = apply dsts p old in
                           (IMap.insert to (new, dsts) trans,
                            q >< Seq.fromList puls)

part2 :: Input -> Int
part2 input = foldr1 lcm gearCycles
  where (transv, intern, inputMap) = internalise input
        start = intern Map.! "broadcaster"
        [(gatekeeper,_)] = inputMap Map.! (-1) -- rx doesn't have a module, thus it is -1
        gears = Map.keys $ inputMap Map.! gatekeeper

        findGearCycles i [] _ = []
        findGearCycles i gears ((from, to, pul) : rest)
          | to == start                              = findGearCycles (i+1) gears rest
          | pul, to == gatekeeper, from `elem` gears = i : findGearCycles i gears' rest
          | otherwise                                = findGearCycles i gears rest
          where gears' = L.delete from gears

        trans = IMap.fromList $ V.toList $ V.indexed transv
        gearCycles = findGearCycles 0 gears $ keepPressing start trans


answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
