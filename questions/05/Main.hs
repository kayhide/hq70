coins :: [Int]
coins = [500, 100, 50, 10]
-- coins = [500, 100]

pattern :: Int -> [Int] -> [[Int]]
pattern 0 _ = [[]]
pattern _ [] = []
pattern n (x:xs) =
    (map (x:) (pattern (n - x) (x:xs))) ++
    (pattern n xs)

limit :: Int -> [[Int]] -> [[Int]]
limit n = filter ((<=n) . length)

main :: IO ()
main = do
    putStrLn $ show $ length $ limit 15 $ pattern 1000 coins
    -- putStrLn $ show $ limit 10 $ pattern 1000 coins



