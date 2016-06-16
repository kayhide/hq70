type Pos = (Int, Int)

type Route = [Pos]

patterns :: Int -> Route -> [Route]
patterns 0 route = [route]
patterns i route = foldl (++) [] $ map (patterns j) $ map (:route) $ nextPoss route
    where
        j = i - 1

nextPoss :: Route -> [Pos]
nextPoss route@((x,y):_) = filter (not . (`elem` route)) candidates
    where
        candidates = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

count :: Int -> Route -> Int
count 0 route = 1
count i route = foldl (+) 0 $ map (count j) $ map (:route) $ nextPoss route
    where
        j = i - 1

main :: IO ()
main = do
    putStrLn $ show $ count 12 [(0, 0)]
-- main = do
--     putStrLn $ show $ length $ patterns 12 [(0, 0)]
