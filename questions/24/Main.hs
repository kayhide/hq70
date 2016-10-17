import Data.List
import Data.Monoid

data Shot = Single Int | Double Int Int deriving (Show)

type Possibilities = [Shot]
type Sequence = [Shot]

allShots :: Possibilities
allShots = (Single <$> [1..9]) ++
           ((uncurry Double) <$> [(1, 2), (1, 4), (2, 3), (3, 6), (4, 7), (6, 9), (7, 8), (8, 9)])
-- allShots = (Single <$> [1..3]) ++ [Double 1 2, Double 2 3]

-- |
-- >>> numbers (Single 2)
-- [2]
-- >>> numbers (Double 2 3)
-- [2,3]
numbers :: Shot -> [Int]
numbers (Single x) = [x]
numbers (Double x y) = [x, y]

-- |
-- >>> isOverlapping (Single 2) (Double 2 3)
-- True
-- >>> isOverlapping (Double 2 3) (Double 3 6)
-- True
isOverlapping :: Shot -> Shot -> Bool
isOverlapping x y = (not . null) $ intersect (numbers x) (numbers y)

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

main :: IO ()
main = print $ getSum $ pitchM allShots
