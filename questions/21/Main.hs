type PascalRow = [Bool]
type PascalStack = [PascalRow]

xor :: Bool -> Bool -> Bool
xor x y = (x && (not y)) || ((not x) && y)

nextPascal :: PascalRow -> PascalRow
nextPascal row = zipWith xor (False:row) (row ++ [False])

countFalse :: PascalStack -> Int
countFalse = sum . fmap (length . filter not)

run :: PascalStack -> PascalStack
run stack@(x:xs)
  | (countFalse stack) < 2014 = run $ (nextPascal x):stack
  | otherwise = stack

main :: IO ()
main = do
  print $ length $ run [[True]]
