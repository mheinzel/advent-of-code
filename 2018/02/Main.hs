module Main where

import Test.HUnit

one :: String -> Int
one = const 0

two :: String -> Int
two = const 0

main = do
  runTests "one" one fooTests
  runTests "two" two barTests
  input <- readFile "input.txt"
  print $ one input
  print $ two input

runTests
  :: (Show a, Eq b, Show b)
  => String -> (a -> b) -> [(a, b)] -> IO Counts
runTests name f ts =
  runTestTT $ TestList $
    zipWith TestLabel (map ((name ++) . show) [1..]) $
      flip map ts $ \(input, expected) ->
        TestCase $
          assertEqual (show input) expected (f input)

fooTests =
  []

barTests =
  []
