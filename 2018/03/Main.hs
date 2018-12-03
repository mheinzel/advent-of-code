{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Test.HUnit

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Foldable
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Safe

one :: String -> Int
one
  = length . Map.filter ((> 1) . length)
  . aggregate
  . foldMap affectedTiles
  . parseClaims

aggregate :: (Ord k, Ord v) => [(k, v)] -> Map.Map k (Set.Set v)
aggregate
  = Map.unionsWith Set.union
  . map (uncurry Map.singleton . fmap Set.singleton)

affectedTiles :: Claim -> [((Int, Int), Int)]
affectedTiles Claim{..}
  = map (, claimId) $
      (,) <$> take sizeX [posX..] <*> take sizeY [posY..]

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



two :: String -> [Int]
two str
  = Set.toList
  . Set.difference (Set.fromList (map claimId claims))
  . fold . Map.elems
  . Map.filter ((> 1) . length)
  . aggregate
  . foldMap affectedTiles
  $ claims

  where
    claims = parseClaims str

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
