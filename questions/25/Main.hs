import Data.List
import Control.Monad

type Eyelet = Int
type Line = (Eyelet, Eyelet)
type LinePattern = [Line]

eyeletsCount :: Int
eyeletsCount = 6

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (fmap (x:) (combinations (n - 1) xs)) ++ (combinations n xs)

patterns :: [LinePattern]
patterns = do
    ls <- permutations [1..(eyeletsCount - 1)]
    rs <- permutations [1..(eyeletsCount - 1)]
    return $ toRight (0:ls) (rs ++ [0])

toRight :: [Eyelet] -> [Eyelet] -> LinePattern
toRight [] _ = []
toRight (l:ls) rights@(r:rs) = (l, r) : (toLeft ls rights)

toLeft :: [Eyelet] -> [Eyelet] -> LinePattern
toLeft [] _ = []
toLeft lefts@(l:ls) (r:rs) = (l, r) : (toRight lefts rs)

intersectionCount :: LinePattern -> Int
intersectionCount lines = sum $ do
    [x, y] <- combinations 2 lines
    guard $ ((fst x) - (snd x)) * ((fst y) - (snd y)) < 0
    return 1

intersections :: LinePattern -> [(Line, Line)]
intersections lines = do
    [x, y] <- combinations 2 lines
    guard $ ((fst x) - (snd x)) * ((fst y) - (snd y)) < 0
    return (x, y)

main :: IO ()
main = do
    print $ maximum $ map intersectionCount patterns
