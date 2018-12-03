{-# LANGUAGE RecordWildCards #-}
module Main where

import Test.HUnit

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

-- one :: String -> Int
one
  = length . Map.filter (> 1) . count . foldMap affectedTiles . parseClaims

count :: (Foldable f, Ord a) => f a -> Map.Map a Int
count
  = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty

affectedTiles :: Claim -> [(Int, Int)]
affectedTiles Claim{..}
  = (,) <$> take sizeX [posX..] <*> take sizeY [posY..]

data Claim
  = Claim { claimId, posX, posY, sizeX, sizeY :: !Int }
  deriving (Eq, Show)

parseClaims :: String -> [Claim]
parseClaims
  = either (error . show) id . P.parse (P.endBy claimP P.newline) ""

claimP :: Parser Claim
claimP
  = do
      P.char '#'
      claimId <- int
      P.string " @ "
      posX <- int
      P.char ','
      posY <- int
      P.string ": "
      sizeX <- int
      P.char 'x'
      sizeY <- int
      pure Claim{..}
  where
    int = read <$> P.many1 P.digit



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
