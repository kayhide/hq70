spotCount :: Int
spotCount = 16

step :: [Int] -> Int
step xs = sum $ zipWith (*) xs $ reverse xs

run :: Int -> [Int]
run 0 = [1]
run n = (step xs) : xs
  where xs = run $ n - 2

main :: IO ()
main = do
  print $ head $ run spotCount
