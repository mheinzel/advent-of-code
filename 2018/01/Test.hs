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
  [ ("+1\n+1\n+1", 3)
  , ("+1\n+1\n-2", 0)
  , ("-1\n-2\n-3", -6)
  ]

bar =
  zipWith TestLabel (map (("bar" ++) . show) [1..]) $
    flip map barTests $ \(input, output) ->
      TestCase $
        assertEqual input output (Main.bar input)

barTests =
  [ ("+1\n-1", 0)
  , ("+3\n+3\n+4\n-2\n-4", 10)
  , ("-6\n+3\n+8\n+5\n-6", 5)
  , ("+7\n+7\n-2\n-7\n-4", 14)
  ]
