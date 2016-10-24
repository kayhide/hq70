import Data.List

data Cell = Void | Black | White deriving (Eq)

instance Show Cell where
  show Void = "□"
  show Black = "◎"
  show White = "◯"

type Position = (Int, Int)
type Board = [(Position, Cell)]

size = (3, 2)
width = fst size
height = snd size

startPosition = (0, 0)
goalPosition = (width - 1, height - 1)

goalBoard :: Board
goalBoard = [ (goalPosition, Black)
            , ((fst goalPosition - 1, snd goalPosition), Void)
            ]

cellAt :: Board -> Position -> Maybe Cell
cellAt board pos = lookup pos board

positionOf :: Board -> Cell -> Maybe Position
positionOf board cell = fst <$> find ((==cell) . snd) board

formatBoard :: Board -> String
formatBoard board = unlines $ do
  y <- [0..(height -1)]
  return $ concat $ do
    x <- [0..(width - 1)]
    let cell = cellAt board (x, y)
      in
      return $ cellToString cell

cellToString :: Maybe Cell -> String
cellToString (Just cell) = show cell
cellToString Nothing = show White

displayBoard :: Board -> IO ()
displayBoard = putStr . formatBoard

main :: IO ()
main = do
    putStrLn "answer"
