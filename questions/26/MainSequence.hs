import Data.List
import Data.Maybe
import Data.Sequence (viewl, fromList, singleton, (><), Seq, ViewL ((:<)))
import Control.Monad

type Position = (Int, Int)
type Size = (Int, Int)

data Motion = East | North | West | South deriving (Eq, Show, Enum)
data Board = Board Position Position deriving (Eq, Show)

type Path = [Motion]
type PathAndBoard = (Path, Board)
type Queue = Seq PathAndBoard

-- size = (3, 2)
size = (10, 10)

startPosition = (0, 0)
goalPosition = (width - 1, height - 1)
  where width = fst size
        height = snd size

startBoard :: Board
startBoard = Board startPosition goalPosition

move :: Position -> Motion -> Position
move (x, y) East = (x + 1, y)
move (x, y) North = (x, y - 1)
move (x, y) West = (x - 1, y)
move (x, y) South = (x, y + 1)

isIn :: Position -> Size -> Bool
isIn (x, y) (w, h) = 0 <= x && x < w && 0 <= y && y < h

isGoaled :: Board -> Bool
isGoaled (Board posBlack _) = posBlack == goalPosition

moveVoid :: Board -> Motion -> Maybe Board
moveVoid (Board posBlack posVoid) mo
  | posVoid' == posBlack = Just (Board posVoid posVoid')
  | posVoid' `isIn` size = Just (Board posBlack posVoid')
  | otherwise = Nothing
  where posVoid' = move posVoid mo

solve :: [Board] -> (Board -> Bool) -> Queue -> PathAndBoard
solve cache pred queue
  | pred board = head
  | otherwise = solve cache' pred tail'
  where (head :< tail) = viewl queue
        (path, board) = head
        next' = catMaybes $ do
          mo <- enumFrom East
          return $ do
            board' <- moveVoid board mo
            guard $ notElem board' cache
            return ((mo:path), board')
        cache' = fmap snd next' ++ cache
        tail' = tail >< fromList next'

main :: IO ()
main = print $ length $ fst $ solve [startBoard] isGoaled $ singleton ([], startBoard)
