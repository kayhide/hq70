type Pos = (Int, Int)

type Route = [Pos]

patterns :: Int -> Route -> [Route]
patterns i route@(pos@(x,y):poss)
    | pos `elem` poss = []
    | i == 0 = [route]
    | otherwise =
      (patterns j $ (x - 1, y) : route) ++
      (patterns j $ (x + 1, y) : route) ++
      (patterns j $ (x, y - 1) : route) ++
      (patterns j $ (x, y + 1) : route)
    where
        j = i - 1

main :: IO ()
main = do
    putStrLn $ show $ patterns 12 [(0, 0)]
