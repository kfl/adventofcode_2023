{-# LANGUAGE Strict #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VU
import Data.Function (on)
import Control.Monad (forM_)

test =  parse "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
input = parse . L.dropWhileEnd (== '\n') <$> readFile "input.txt"

type Input = [String]

parse = L.splitOn ","

aocHASH = loop 0
  where loop curr [] = curr
        loop curr (c : rest) = loop (((curr + C.ord c) * 17) `rem` 256) rest

part1 :: Input -> Int
part1 input = sum $ map aocHASH input
answer1 = part1 <$> input

type Label = String
data Operation = Focal Int
               | Delete
  deriving (Eq, Show)
type Step = (Label, Operation)
type Box = [Step]

parseStep :: String -> Step
parseStep str = (lab, oper)
  where (lab, rest) = span C.isAlpha str
        oper = case rest of
                 ('=' : rest) -> Focal $ read rest
                 "-"          -> Delete

apply :: Step -> Box -> Box
apply step@(lab, oper) box =
  case oper of
    Delete -> L.deleteBy ((==) `on` fst) step box
    Focal n -> pre ++ step : drop 1 post
      where (pre, post) = break ((lab ==) . fst) box


applySteps steps = V.modify $ \boxes -> do
  forM_ steps $ \step@(lab,_) -> do
    let i = aocHASH lab
    VU.modify boxes (apply step) i

focusingPower boxes = V.sum $ V.imap power boxes
  where power i box = sum [ (i+1) * j * n | (j, (_, Focal n)) <- zip [1..] box ]

part2 :: Input -> Int
part2 input = focusingPower $ applySteps steps boxes
  where steps = map parseStep input
        boxes = V.replicate 256 []

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
