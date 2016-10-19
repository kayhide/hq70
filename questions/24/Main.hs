import Data.List
import Data.Monoid

data Shot = Single Int | Double Int Int deriving (Show)

type Possibilities = [Shot]
type Sequence = [Shot]

allShots :: Possibilities
allShots = singles ++ doubles
  where singles = Single <$> [1..9]
        doubles = (uncurry Double) <$> (zipWith (,) nums $ drop 1 $ cycle nums)
        nums = [1, 2, 3, 6, 9, 7, 8, 4]
-- allShots = (Single <$> [1..3]) ++ [Double 1 2, Double 2 3]

-- |
-- >>> isOverlapping (Single 2) (Double 2 3)
-- True
-- >>> isOverlapping (Double 2 3) (Double 3 6)
-- True
isOverlapping :: Shot -> Shot -> Bool
isOverlapping (Single x) (Single y) = x == y
isOverlapping (Single x) (Double y1 y2) = x == y1 || x == y2
isOverlapping (Double x1 x2) (Single y) = x1 == y || x2 == y
isOverlapping (Double x1 x2) (Double y1 y2) = x1 == y1 || x1 == y2 || x2 == y1 || x2 == y2

-- |
-- >>> except (Single 2) [Single 1,Single 2,Double 2 3]
-- [Single 1]
-- >>> except (Double 1 2) [Single 1,Single 2,Double 2 3]
-- []
except :: Shot -> Possibilities -> Possibilities
except x = filter (not . (isOverlapping x))

-- |
-- >>> pitch [Single 1,Single 2]
-- [[Single 1,Single 2],[Single 2,Single 1]]
-- >>> pitch [Single 1,Single 2,Double 1 2]
-- [[Single 1,Single 2],[Single 2,Single 1],[Double 1 2]]
pitch :: Possibilities -> [Sequence]
pitch [] = [[]]
pitch shots = do
  x <- shots
  fmap (x:) $ pitch (except x shots)

main' :: IO ()
main' = print $ length $ pitch allShots

-- |
-- >>> pitchM [Single 1,Single 2]
-- Sum {getSum = 2}
-- >>> pitchM [Single 1,Single 2,Double 1 2]
-- Sum {getSum = 3}
pitchM :: Possibilities -> Sum Int
pitchM [] = Sum 1
pitchM shots = mconcat $ do
  x <- shots
  return $ pitchM (except x shots)

main'' :: IO ()
main'' = print $ getSum $ pitchM allShots

-- |
-- >>> numbers (Single 2)
-- [2]
-- >>> numbers (Double 2 3)
-- [2,3]
numbers :: Shot -> [Int]
numbers (Single x) = [x]
numbers (Double x y) = [x, y]

-- |
-- >>> isCompleted 2 [Single 1,Single 2]
-- True
-- >>> isCompleted 2 [Double 1 2]
-- True
-- >>> isCompleted 2 [Single 1]
-- False
isCompleted :: Int -> Possibilities -> Bool
isCompleted n shots = n == length nums
  where nums = mconcat $ fmap numbers shots

-- |
-- >>> shotCombinations [Single 1,Single 2,Double 1 2]
-- [[Single 1,Single 2],[Double 1 2]]
shotCombinations :: Possibilities -> [Possibilities]
shotCombinations [] = [[]]
shotCombinations shots@(x:xs) = filter (isCompleted count) combs
  where count = length $ nub $ mconcat $ fmap numbers shots
        combs = fmap (x:) (shotCombinations (except x xs)) ++ shotCombinations xs

fact :: Integral a => a -> a
fact n = foldl (*) 1 [1..n]

main :: IO ()
main = print $ sum $ do
  nums <- group $ sort $ fmap length $ shotCombinations allShots
  return $ (length nums) * (fact (head nums))
