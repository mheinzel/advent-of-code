module Main where

import Test.HUnit

import qualified Data.Array as Arr
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Applicative
import Control.Arrow ((&&&))
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Function (on)
import Safe

type Coord = (Int, Int)

parse :: String -> [Coord]
parse = map parseLine . lines
  where
    parseLine str
      = let
          (str1, rest) = span Char.isDigit str
          (_, str2) = span (not . Char.isDigit) rest
        in
          (read str1, read str2)

one :: String -> Int
one 
  = largest . removeInfinites . closests . parse

bounds = (400,400)

closests :: [Coord] -> Arr.Array Coord (Maybe Coord)
closests coords
  = Arr.listArray ((0,0), bounds) $
      closest coords <$> liftA2 (,) [0 .. fst bounds] [0 .. snd bounds]

closest :: [Coord] -> Coord -> Maybe Coord
closest coords to
  = case sortGroup (distance to) coords of
      ((_, [c]) : _) -> Just c
      _ -> Nothing

sortGroup :: Ord k => (a -> k) -> [a] -> [(k, [a])]
sortGroup key
  = groupOn' fst . sortOn fst . map (annotate key)

  where
    annotate f x
      = (f x, x)

    groupOn' f
      = map (fst . head &&& map snd)
      . groupBy ((==) `on` f)

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2)
  = abs (x2 - x1) + abs (y2 - y1)

removeInfinites
  :: (Arr.Ix i, Eq a)
  => Arr.Array (i, i) (Maybe a) -> Arr.Array (i, i) (Maybe a)
removeInfinites arr
  = guard (`notElem` edgeElems arr) <$> arr
  where
    guard p x
      | p x = x
      | otherwise = Nothing

edgeElems :: Arr.Ix i => Arr.Array (i, i) a -> [a]
edgeElems arr
  = map snd $ filter isOnEdge $ Arr.assocs arr
  where
    ((minX, minY), (maxX, maxY)) = Arr.bounds arr
    isOnEdge ((x, y), _)
      = x `elem` [minX, maxX] || y `elem` [minY, maxY]

largest :: (Arr.Ix i, Ord a) => Arr.Array i (Maybe a) -> Int
largest
  = maximum . map length . group . sort . foldMap toList

two :: String -> Int
two
  = const 0

main = do
  runTests "one" one oneTests
  runTests "two" two twoTests
  input <- readFile "input.txt"
  print $ one input
  print $ two input

oneTests =
  []

twoTests =
  []

runTests
  :: (Show a, Eq b, Show b)
  => String -> (a -> b) -> [(a, b)] -> IO Counts
runTests name f ts =
  runTestTT $ TestList $
    zipWith TestLabel (map ((name ++) . show) [1..]) $
      flip map ts $ \(input, expected) ->
        TestCase $
          assertEqual (show input) expected (f input)
