{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Foldable (asum)
import Data.Maybe (fromJust)

import Text.Printf
import System.Process (callCommand)


test =  parse $ unlines [ "px{a<2006:qkq,m>2090:A,rfg}"
                        , "pv{a>1716:R,A}"
                        , "lnx{m>1548:A,A}"
                        , "rfg{s<537:gd,x>2440:R,A}"
                        , "qs{s>3448:A,lnx}"
                        , "qkq{x<1416:A,crn}"
                        , "crn{x>2662:A,R}"
                        , "in{s<1351:px,qqz}"
                        , "qqz{s>2770:qs,m<1801:hdj,R}"
                        , "gd{a>3333:R,R}"
                        , "hdj{m>838:A,pv}"
                        , ""
                        , "{x=787,m=2655,a=1222,s=2876}"
                        , "{x=1679,m=44,a=2067,s=496}"
                        , "{x=2036,m=264,a=79,s=2244}"
                        , "{x=2461,m=1339,a=466,s=291}"
                        , "{x=2127,m=1623,a=2188,s=1013}"
                        ]
input = parse <$> readFile "input.txt"

type Name = String
type Rating = (Int, Int, Int, Int)
data Selector = X | M | A | S
  deriving (Eq, Show, Read, Ord, Enum)
data Operator = Less | Greater
  deriving (Eq, Show, Ord, Enum)
type Condition = Either Name (Selector, Operator, Int, Name)
type Rule = (String, [Condition])

type ConditionFun = Rating -> Maybe String

type Input = ([Rule], [Rating])

xp (x,m,a,s) = x
mp (x,m,a,s) = m
ap (x,m,a,s) = a
sp (x,m,a,s) = s

view = \case X -> xp; M -> mp; A -> ap; S -> sp

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

parse :: String -> Input
parse str = (rules, ratings)
  where [ruless, ratingss] = L.splitOn "\n\n" str
        rating s = read $ "(" ++ filter (\c -> C.isDigit c || c ==',') s ++ ")"
        ratings = map rating $ lines ratingss
        rules = map rule $ lines ruless
        rule s = (name, conds)
          where
            (name, condss) = break (== '{') s
            conds = map cond $ L.splitOn "," $ filter (`notElem` "{}") condss

        cond s = case break (== ':') s of
                   (_, []) -> Left s
                   (f : opr : n, _ : dst) -> Right (read [C.toUpper f], oper opr, read n, dst)
        oper= \case '>' -> Greater ; '<' -> Less

toFun :: Condition -> ConditionFun
toFun = \case
  Left dst -> const $ pure dst
  Right (s, o, n, dst) -> \r -> toMaybe (oper (sel r) n) dst
    where oper = case o of Less -> (<); Greater -> (>)
          sel = view s

ruleMap rules = Map.fromList $ ("A", accept) : ("R", reject) :
                               [ (n, trans) | (n, conds) <- rules,
                                 let cf = map toFun conds
                                     trans = pure . fromJust . asum . (cf <*>) . L.singleton]
  where accept = const $ Left True
        reject = const $ Left False

apply rm s r = loop s
  where loop s = case (rm ! s) r of
                   Left fin -> fin
                   Right next -> loop next

part1 :: Input -> Int
part1 (rules, ratings) = sum $ map fourSum accepted
  where rm = ruleMap rules
        accepted = filter (apply rm "in") ratings
        fourSum (x,m,a,s) = x+m+a+s

answer1 = part1 <$> input

showRules name rules = do
  let cond from = \case
        Left dst -> printf "  %s -> %s;\n" from dst
        Right (sel,opr,n,dst) -> printf "  %s -> %s;\n" from dst
      trans = [ tran | (n, conds) <- rules,
                       tran <- map (cond n) conds]
      dotfile = name ++ ".dot"
      pdffile = name ++ ".PDF"
      pre = "digraph rules {overlap=\"prism\" \n "++
            "A [style=\"rounded,filled\",fillcolor=green]; "++
            "R [style=\"rounded,filled\",fillcolor=grey]; " ++
            "in [style=\"rounded,filled\",fillcolor=red]; \n"
  writeFile dotfile $ concat $ pre : trans ++ ["}\n"]
  callCommand $ "dot -Tpdf "++dotfile ++" -o "++pdffile
  callCommand $ "open -a Preview "++pdffile

ruleGraph rules = Map.fromList $ ("A", []) : ("R", []) : rules

type Bound = (Int, Int)
type Domain = (Bound, Bound, Bound, Bound)

neg = \case
  (Less, n) -> (Greater, n-1)
  (Greater, n) -> (Less, n+1)

addConstraint con (lower, upper) =
  case con of
    (Less, n) -> (lower, min n upper)
    (Greater, n) -> (max n lower, upper)

modifyx (x,m,a,s) f = (f x, m, a, s)
modifym (x,m,a,s) f = (x, f m, a, s)
modifya (x,m,a,s) f = (x, m, f a, s)
modifys (x,m,a,s) f = (x, m, a, f s)

over = \case X -> modifyx; M -> modifym; A -> modifya; S -> modifys

bounds (low1, upper1) (low2, upper2) = (max low1 low2, min upper1 upper2)

combine xmas1 xmas2 = (xp xmas1 `bounds` xp xmas2,
                       mp xmas1 `bounds` mp xmas2,
                       ap xmas1 `bounds` ap xmas2,
                       sp xmas1 `bounds` sp xmas2)

fullDomain :: Domain
fullDomain = ((0, 4001),(0, 4001),(0, 4001),(0, 4001))
emptyDomain = ((0, 0),(0, 0),(0, 0),(0, 0))

allPaths rg = snd . loop initial
  where initial =
          Map.fromList [ ("A", [ fullDomain ])
                       , ("R", [ emptyDomain ]) ]
        loop seen s =
          case Map.lookup s seen of
            Just res -> (seen, res)
            _ -> (seen'', res)
          where ((_, seen'), paths) = L.mapAccumL collect (fullDomain, seen) $ rg ! s
                res = concat paths
                seen'' = Map.insert s res seen'

        collect (negated, seen) = \case
          Left dst ->
            let (seen', res) = loop seen dst
            in  ((negated, seen'), map (combine negated) res)
          Right (sel, opr, n, dst) ->
            let (seen', res) = loop seen dst
            in  ((over sel negated $ addConstraint $ neg (opr, n), seen'),
                 map (combine (over sel negated $ addConstraint (opr, n))) res)

bsize (lower, upper) | lower < upper = upper - lower - 1
                     | otherwise = 0
dsize (x,m,a,s) = bsize x * bsize m * bsize a * bsize s

part2 :: Input -> Int
part2 (rules, _) = sum $ map dsize paths
  where rg = ruleGraph rules
        paths = allPaths rg "in"
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
