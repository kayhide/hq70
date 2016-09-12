import Data.List
import Data.Function
import Control.Monad

type Summation = Int
type Counting = Int
type SumCount = (Summation, Counting)

allNums :: [Int]
allNums = [1, 14, 14, 4, 11, 7, 6, 9, 8, 10, 10, 5, 13, 2, 3, 15]

combinationAll :: [a] -> [[a]]
combinationAll [] = [[]]
combinationAll (x:xs) = [id, (x:)] <*> (combinationAll xs)

groups :: [[Summation]]
groups = group $ sort $ fmap sum $ combinationAll allNums

counts :: [SumCount]
counts = do
  g <- groups
  return (head g, length g)


type CountList = [Int]

shiftAdd :: CountList -> Int -> CountList
shiftAdd xs n = zipWith (+) (xs ++ zeros) (zeros ++ xs)
  where zeros = replicate n 0

totalCounts :: CountList
totalCounts = foldl shiftAdd [1] allNums

counts' :: [SumCount]
counts' = zipWith (,) [0..] totalCounts

main :: IO ()
main = do
  print $ maximumBy (compare `on` snd) counts'
