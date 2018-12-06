{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Foldable
import Safe

one :: String -> Int
one
  = length . reduce
  . filter Char.isAlpha

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
  = minimum . map (length . reduce)
  . simplifications
  . filter Char.isAlpha

simplifications :: [Char] -> [[Char]]
simplifications
  = traverse simplify ['a'..'z']

simplify :: Char -> [Char] -> [Char]
simplify c
  = filter ((/= c) . Char.toLower)

main = do
  input <- readFile "input.txt"
  print $ one input
  print $ two input
