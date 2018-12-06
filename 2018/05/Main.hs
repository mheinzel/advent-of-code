{-# LANGUAGE LambdaCase #-}
module Main where

import Test.HUnit

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Foldable
import Safe

one :: String -> Int
one
  = length . reduce . filter Char.isAlpha

reduce :: [Char] -> [Char]
reduce = flip foldl' [] $ \case
  [] -> pure
  (x:xs) -> \c -> react c x ++ xs

react :: Char -> Char -> [Char]
react x y
  | Char.toLower x == Char.toLower y
  , x /= y
  = []

  | otherwise
  = [x, y]

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
