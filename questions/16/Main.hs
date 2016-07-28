import Control.Monad
import Data.List
import Data.Function

type Length = Int
type Area = Int
type Tri = (Area, Area, Area)

allLengths :: [Length]
allLengths = [1..500]

isSquareValid :: Length -> Bool
isSquareValid len = len `mod` 4 == 0

areaVariations :: Length -> [Area]
areaVariations len = do
    x <- [1..(half `div` 2 - 1)]
    pure $ x * (half - x)
    where half = len `div` 2

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (fmap (x:) (combinations (n - 1) xs)) ++ (combinations n xs)

validTris :: Length -> [Tri]
validTris len = do
    [x, y] <- combinations 2 areas
    guard $ x + y == squareArea
    pure (squareArea, x, y)
    where squareArea = (len `div` 4) ^ 2
          areas = areaVariations len

exceptSimilar :: [Tri] -> [Tri]
exceptSimilar = nubBy ((==) `on` regularize)
    where regularize (a, x, y) =
              (fromIntegral x / fromIntegral a, fromIntegral y / fromIntegral a)

run :: [Tri]
run = do
    len <- allLengths
    guard $ isSquareValid len
    validTris len

run' :: [Tri]
run' = do
    len <- allLengths
    guard $ isSquareValid len
    let quarter = len `div` 4
    delta1 <- [1..(quarter - 1)]
    delta2 <- [delta1..(quarter - 1)]
    let quarterSquared = quarter ^ 2
        delta1Squared = delta1 ^ 2
        delta2Squared = delta2 ^ 2
    guard $ delta1Squared + delta2Squared == quarterSquared
    pure (quarterSquared, quarterSquared - delta1Squared, quarterSquared - delta2Squared)

main :: IO ()
main = print $ length $ exceptSimilar run'

