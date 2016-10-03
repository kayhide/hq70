type PascalRow = [Bool]
type PascalStack = [PascalRow]

-- |
-- >>> xor True True
-- False
-- >>> xor True False
-- True
-- >>> xor False True
-- True
-- >>> xor True True
-- False
xor :: Bool -> Bool -> Bool
xor x y = (x && (not y)) || ((not x) && y)

-- |
-- >>> nextPascal [True]
-- [True,True]
-- >>> nextPascal [True,True]
-- [True,False,True]
nextPascal :: PascalRow -> PascalRow
nextPascal row = zipWith xor (False:row) (row ++ [False])

-- |
-- >>> countFalse []
-- 0
-- >>> countFalse [[False],[True,False]]
-- 2
countFalse :: PascalStack -> Int
countFalse = sum . fmap (length . filter not)

-- |
-- >>> run 1 [[True]]
-- [[True,False,True],[True,True],[True]]
-- >>> run 3 [[True]]
-- [[True,False,False,False,True],[True,True,True,True],[True,False,True],[True,True],[True]]
run :: Int -> PascalStack -> PascalStack
run n stack@(x:xs)
  | (countFalse stack) < n = run n $ (nextPascal x):stack
  | otherwise = stack

-- |
-- >>> search 1
-- 3
-- >>> search 3
-- 5
search :: Int -> Int
search n = length $ run n [[True]]

main :: IO ()
main = do
  print $ search 2014
