module Main where

import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Test.HUnit

-- guard and their sleeping minutes (and summed count of slept minutes)
data SleepingBag
  = SleepingBag
  { guard :: Int
  , sleepingMinutes :: [Int]
  }
  deriving (Show, Eq, Ord)

-- for each bag: fold multiple bags into one.
-- reduced, duplicates reduced -> unique guards | Map/Dictionary
foldBags :: [SleepingBag] -> [SleepingBag]
foldBags
  = map foldBag . groupByGuard . sort
  where
    foldBag bs = SleepingBag (guard (head bs)) (mins bs)
    mins = concat . map sleepingMinutes

groupByGuard :: [SleepingBag] -> [[SleepingBag]]
groupByGuard
  = groupBy ((==) `on` guard)

streamToSleepingBags :: [String] -> [SleepingBag]
streamToSleepingBags
  = fillBag [] . sort
  where
    fillBag bs []
      = bs

    fillBag [] (l:ls)
      | isStart l = fillBag [newSleepingBag (guard' l)] ls -- start of new Bag.
      | otherwise = fillBag [] ls -- discard l?

    fillBag (b:bs) (l:ls)
      | isStart l
        = fillBag (newSleepingBag (guard' l) : b : bs) ls

      | isSleep l
        = case ls of
            []     -> fillBag (addSleep b (timeRangeBag (minute l) 59) : bs) ls
            (k:ks) ->
              if isWake k
                 then fillBag (addSleep b (betweenLogs l k) : bs) ks
                 else fillBag (b:bs) (ks) -- discard k

      | otherwise
        = fillBag (b:bs) (ls) -- discard l

    newSleepingBag :: Int -> SleepingBag
    newSleepingBag g
      = SleepingBag g []

    addSleep :: SleepingBag -> [Int] -> SleepingBag
    addSleep (SleepingBag g smins) amins
      = SleepingBag g (smins ++ amins)

    timeRangeBag :: Int -> Int -> [Int]
    timeRangeBag s e = [s .. e - 1]

    betweenLogs :: String -> String -> [Int]
    betweenLogs l k = timeRangeBag (minute l) (minute k)

    minute :: String -> Int
    minute msg
      = let t = (take 5 . drop 12) msg
        in case head t of
           '2' -> 0 -- early start is still 0
           _   -> (read . drop 3) t

    body = drop 19
    isStart = (== "Guard #") . take 7 . body
    isSleep = (== "falls asleep") . body
    isWake = (== "wakes up") . body

    guard' msg
      | isStart msg = (read . head . words . drop 7 . body) msg
      | otherwise = 0

one :: String -> Int
one str
  = a * b

  where
    -- 1. Lines to log
    -- 2. Group shifts and show them differently
    -- 3. Map Guard with All Shifts eeever!
    shifts' = (foldBags . streamToSleepingBags . lines)  str

    -- 4. Get Max sleeper
    max :: [SleepingBag] -> SleepingBag
    max
      = foldl max' (SleepingBag 0 [])
      where
        max' (SleepingBag ga ma) (SleepingBag gb mb)
          | length ma > length mb = SleepingBag ga ma
          | otherwise = SleepingBag gb mb

    -- get bag with most items.
    getMostUsed :: [Int] -> Int
    getMostUsed bs
      = case (foldl moreUsed [] . group . sort) bs of
          []     -> 0
          (x:xs) -> x

    moreUsed a b = if length a > length b then a else b

    SleepingBag a b' = max shifts'
    b = getMostUsed b'

two :: String -> Int
two
  = const 0

main = do
  runTests "one" one oneTests
  runTests "two" two twoTests
  input <- readFile "input.txt"
  -- putStrLn $ unlines $ map show $ sort $ streamToSleepingBags $ lines $ fst $ head $ oneTests
  -- putStrLn ""
  -- putStrLn $ unlines $ map show $ foldBags $ streamToSleepingBags $ lines $ fst $ head $ oneTests
  --  print $ foldBag (SleepingBag 10 (bags mins10) 45) $ (SleepingBag 1 (bags mins10') 5)
  --  print $ foldBag (SleepingBag 99 (bags mins99) 10) $ foldBag (SleepingBag 0 (bags mins99') 10) $ (SleepingBag 1 (bags mins99'') 10)
  print $ one $ input
  print $ two $ input

  where
    bags = map (\x -> [x])
    (mins10, mins10') = ([5..25-1] ++ [30..55-1], [24..29-1])
    (mins99, mins99',mins99'') = ([40..50-1], [36..46-1], [45..55-1])

oneTests =
  [("[1518-11-01 00:00] Guard #10 begins shift\n\
    \[1518-11-01 00:05] falls asleep\n\
    \[1518-11-01 00:25] wakes up\n\
    \[1518-11-01 00:30] falls asleep\n\
    \[1518-11-01 00:55] wakes up\n\
    \[1518-11-01 23:58] Guard #99 begins shift\n\
    \[1518-11-02 00:40] falls asleep\n\
    \[1518-11-02 00:50] wakes up\n\
    \[1518-11-03 00:05] Guard #10 begins shift\n\
    \[1518-11-03 00:24] falls asleep\n\
    \[1518-11-03 00:29] wakes up\n\
    \[1518-11-04 00:02] Guard #99 begins shift\n\
    \[1518-11-04 00:36] falls asleep\n\
    \[1518-11-04 00:46] wakes up\n\
    \[1518-11-05 00:03] Guard #99 begins shift\n\
    \[1518-11-05 00:45] falls asleep\n\
    \[1518-11-05 00:55] wakes up"
  , (240))]
  -- , ([], 10,24, 240))]

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
