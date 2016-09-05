import Control.Monad

data Direction = East | North | West | South deriving (Eq, Show, Enum)

type Point = (Int, Int)
type Path = (Point, Direction)
type Route = [Path]

width :: Int
width = 6

height :: Int
height = 4

goal :: Point
goal = (width, height)

step :: Point -> Direction -> Point
step (x, y) East = (x + 1, y)
step (x, y) North = (x, y + 1)
step (x, y) West = (x - 1, y)
step (x, y) South = (x, y - 1)

endPoint :: Path -> Point
endPoint (pt, dir) = step pt dir

flipPath :: Path -> Path
flipPath path@(pt, dir) = (endPoint path, turnBack dir)

allPaths :: [Path]
allPaths = do
  x <- [0..width]
  y <- [0..height]
  dir <- [East, North, West, South]
  let (x1, y1) = step (x, y) dir
  guard $ x1 >= 0 && x1 <= width
  guard $ y1 >= 0 && y1 <= height
  return ((x, y), dir)

turnLeft :: Direction -> Direction
turnLeft East = North
turnLeft North = West
turnLeft West = South
turnLeft South = East

turnBack :: Direction -> Direction
turnBack East = West
turnBack North = South
turnBack West = East
turnBack South = North

straightPath :: Path -> Path
straightPath (p, dir) = (step p dir, dir)

leftPath :: Path -> Path
leftPath (p, dir) = (step p dir, turnLeft dir)

findRoutes :: Route -> [Path] -> [Route]
findRoutes route@(p:_) paths
  | next == goal = [route]
  | otherwise = do
      path <- [straightPath p, leftPath p]
      guard $ path `elem` paths
      findRoutes (path:route) $ filter (\p -> p /= path && p /= flipPath path) paths
      -- findRoutes (path:route) $ filter (\p -> p /= path) paths
  where next = endPoint p

origin :: Path
origin = ((-1, 0), East)

main :: IO ()
main = do
  print $ length $ findRoutes [origin] allPaths
