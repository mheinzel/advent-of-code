module Main where

import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import Safe

foo :: String -> Int
foo =
  sum . mapMaybe toInt . lines

bar :: String -> Int
bar =
  firstDuplicate . scanl (+) 0 . mapMaybe toInt . cycle . lines

toInt :: String -> Maybe Int
toInt ('+' : n) = readMay n
toInt ('-' : n) = negate <$> readMay n
toInt _ = Nothing

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = go Set.empty
  where
    go set (x:xs)
      | x `Set.member` set = x
      | otherwise = go (Set.insert x set) xs

main = do
  input <- readFile "input.txt"
  print $ foo input
  print $ bar input
