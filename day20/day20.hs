{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia, StandaloneDeriving, TypeFamilies #-}
{-# LANGUAGE LambdaCase, Strict, OverloadedLists #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.STRef as ST
import qualified Control.Monad.ST as ST
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..), (><), ViewL(..))
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

internalise :: [Module String] -> (V.Vector (Transfer, [Int]), Map String Int)
internalise modules = (V.fromList [ complete m | m <- modules],
                       internMap)
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

buttonPres start (trans, c, _) = ST.runST $ do
  transm <- V.thaw trans
  counters <- ST.newSTRef c
  rx <- ST.newSTRef (0, 0)
  let q = Seq.singleton (-1, start, False)
  let loop (Seq.viewl -> p@(from, to, pul) :< q) = do
        let sel = if pul then first else second
        ST.modifySTRef counters (sel (+1))
        if to /= -1 then do
          (old, dsts) <- VM.read transm to
          let (new, puls) = apply dsts p old
          VM.write transm to (new, dsts)
          loop (q >< Seq.fromList puls)
        else do
          ST.modifySTRef rx (sel(+1))
          loop q
      loop _ = return ()
  loop q
  c' <- ST.readSTRef counters
  trans' <- V.unsafeFreeze transm
  r <- ST.readSTRef rx
  return (trans', c', r)

part1 :: Input -> Int
part1 input = highs * lows
  where (trans, intern) = internalise input
        start = intern Map.! "broadcaster"
        step = buttonPres start
        steps = iterate step (trans, (0,0), (0,0))
        res@(_, (highs, lows), _) = steps !! 1000
answer1 = part1 <$> input

--part2 :: Input -> Int
part2 input = L.findIndex (\(_,_, (_, rxlow)) -> rxlow == 1) $ take 100_000_000 steps
  where (trans, intern) = internalise input
        start = intern Map.! "broadcaster"
        step = buttonPres start
        steps = iterate step (trans, (0,0), (0,0))

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
