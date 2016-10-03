type PascalRow = [Bool]
type PascalStack = [PascalRow]

xor :: Bool -> Bool -> Bool
xor x y = (x && (not y)) || ((not x) && y)

nextPascal :: PascalRow -> PascalRow
nextPascal row = zipWith xor (False:row) (row ++ [False])

countFalse :: PascalStack -> Int
countFalse = sum . fmap (length . filter not)

run :: Int -> PascalStack -> PascalStack
run n stack@(x:xs)
  | (countFalse stack) < n = run n $ (nextPascal x):stack
  | otherwise = stack

search :: Int -> Int
search n = length $ run n [[True]]

main :: IO ()
main = do
  print $ search 2014
