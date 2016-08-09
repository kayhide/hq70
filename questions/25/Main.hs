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

patterns :: Int -> [LinePattern]
patterns n = do
    ls <- permutations [1..(n - 1)]
    rs <- permutations [1..(n - 1)]
    return $ toRight (0:ls) (rs ++ [0])

toRight :: [Eyelet] -> [Eyelet] -> LinePattern
toRight [] _ = []
toRight (l:ls) rights@(r:_) = (l, r) : (toLeft ls rights)

toLeft :: [Eyelet] -> [Eyelet] -> LinePattern
toLeft [] _ = []
toLeft lefts@(l:_) (r:rs) = (l, r) : (toRight lefts rs)

crossing :: Line -> Line -> Bool
crossing (l1, r1) (l2, r2) = (l1 - l2) * (r1 - r2) < 0

intersections :: LinePattern -> [(Line, Line)]
intersections lines = do
    [x, y] <- combinations 2 lines
    guard $ crossing x y
    return (x, y)

main :: IO ()
main = do
    print $ maximum $ fmap (length . intersections) $ patterns eyeletsCount
