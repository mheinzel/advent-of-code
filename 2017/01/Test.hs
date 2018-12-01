module Test where

import qualified Main
import Test.HUnit

main = runTestTT $ TestList $
  foo ++ bar

foo =
  zipWith TestLabel (map (("foo" ++) . show) [1..]) $
    flip map fooTests $ \(input, output) ->
      TestCase $
        assertEqual input output (Main.foo input)

fooTests =
  [ ("1122", 3)
  , ("1111", 4)
  , ("1234", 0)
  , ("91212129", 9)
  ]

bar =
  zipWith TestLabel (map (("bar" ++) . show) [1..]) $
    flip map barTests $ \(input, output) ->
      TestCase $
        assertEqual input output (Main.bar input)

barTests =
  [ ("1212", 6)
  , ("1221", 0)
  , ("123425", 4)
  , ("123123", 12)
  , ("12131415", 4)
  ]
