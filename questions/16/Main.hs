type Length = Int
type Area = Int

allLengths :: [Length]
allLengths = [1..500]

isSquareValid :: Length -> Bool
isSquareValid len = len `mod` 4 == 0

exceptSimilar :: [Int] -> [Int]
exceptSimilar [] = []
exceptSimilar (x:xs) =
    x : (filter ((/= 0) . (`mod` x)) (exceptSimilar xs))

allLengthsOfSquareValid :: [Length]
allLengthsOfSquareValid = filter isSquareValid allLengths

areaVariations :: Length -> [Area]
areaVariations len = fmap area [1..(half `div` 2 - 1)]
    where half = len `div` 2
          area x = x * (half - x)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (fmap (x:) (combinations (n - 1) xs)) ++ (combinations n xs)

sumVariations :: [Area] -> [Area]
sumVariations = (fmap sum) . (combinations 2)

isValidLength :: Length -> Bool
isValidLength len = squareArea `elem` (sumVariations areas)
    where squareEdge = len `div` 4
          squareArea = squareEdge * squareEdge
          areas = areaVariations len

main :: IO ()
main = do
    print $ filter isValidLength $ exceptSimilar allLengthsOfSquareValid
