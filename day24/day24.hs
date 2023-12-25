{-# LANGUAGE Strict #-}
module Main where

import qualified Data.List as L
import Data.Ratio qualified as Q
import Data.Ratio ((%))

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Data.SBV as S
import Data.SBV ( (.>=), (.==), (.&&) )


test =  map parse [ "19, 13, 30 @ -2,  1, -2"
                  , "18, 19, 22 @ -1, -1, -2"
                  , "20, 25, 34 @ -2, -2, -4"
                  , "12, 31, 28 @ -1, -2, -1"
                  , "20, 19, 15 @  1, -5, -3"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Trip = (Integer, Integer, Integer)
type Hailstone = (Trip, Trip)
type Input = [Hailstone]


parse :: String -> (Trip, Trip)
parse str = (trip position, trip velocity)
  where (position, _: velocity) = break (== '@') str
        trip s = read $ "("++s++")"

type Line = (Q.Rational, Q.Rational)

toLine :: Hailstone -> Line
toLine ((x,y,_), (xv,yv,_)) = (slope, c)
  where slope = yv % xv
        c = (y%1) - slope * (x%1)

lineIntersection :: Line -> Line -> Maybe (Q.Rational, Q.Rational)
lineIntersection (slope1, c1) (slope2, c2)
  | slope1 == slope2 = Nothing
  | otherwise = let x = (c2 - c1) / (slope1 - slope2) in pure (x, slope1 * x + c1)

intersection :: Hailstone -> Hailstone -> Maybe (Q.Rational, Q.Rational)
intersection h1@((x1, _, _), (xv1, _, _)) h2@((x2, _, _), (xv2, _, _)) =
  case lineIntersection (toLine h1) (toLine h2) of
    Just p@(px, _) | signumMatch xv1 x1 px && signumMatch xv2 x2 px -> Just p
    _ -> Nothing
  where
    signumMatch v x px = signum (v%1) == signum (px - x%1)

intersectingHailstones low up hailstones = [(h1, h2, p) | (h1:hs) <- L.tails hailstones
                                                        , h2 <- hs
                                                        , Just p@(px,py) <- [intersection h1 h2]
                                                        , lower <= px, px <= upper
                                                        , lower <= py, py <= upper ]
  where (lower, upper) = (low%1, up%1)

part1 :: Input -> Int
part1 input = length $ intersectingHailstones 200000000000000 400000000000000 input
answer1 = part1 <$> input

equations :: [Hailstone] -> IO S.SatResult
equations hailstones = S.sat $ do
  vars@[x,y,z, xv,yv,zv] <- mapM domType ["x", "y", "z", "xv", "yv", "zv"]
  forM_ (zip [0..] hailstones) $ \(i, ((hx,hy,hz), (hxv,hyv,hzv))) -> do
    t <- domType $ "t_"++show i
    S.constrain $ t .>= 0
    S.constrain $ x + xv * t .== fromIntegral hx + fromIntegral hxv * t
    S.constrain $ y + yv * t .== fromIntegral hy + fromIntegral hyv * t
    S.constrain $ z + zv * t .== fromIntegral hz + fromIntegral hzv * t
  return $ x .>= 0 .&& y .>= 0 .&& z .>= 0

  where domType = S.sReal

part2 :: Input -> IO Integer
part2 input = do
  model <- equations $ take 3 input
  let [x,y,z] = map (fromJust . (`S.getModelValue` model)) ["x", "y", "z"]
  case S.algRealToRational $ x + y + z of
    S.RatExact r | Q.denominator r == 1 -> return $ Q.numerator r
    _ -> error "SBV couldn't find an exact integer solution"

answer2 = part2 =<< input

main = do
  inp <- input
  print $ part1 inp
  print =<< part2 inp
