import Data.List
import Data.Ix
import Control.Monad

data Direction = East | North | West | South deriving (Eq, Ord, Show, Enum)

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
flipPath path@(_, dir) = (endPoint path, turnBack dir)

allPaths :: [Path]
allPaths = do
  x <- [0..width]
  y <- [0..height]
  dir <- [East .. South]
  let (x1, y1) = step (x, y) dir
  guard $ inRange (0, width) x1
  guard $ inRange (0, height) y1
  return ((x, y), dir)

turn :: Int -> Direction -> Direction
turn angle = toEnum . (`mod` 4) . (+ angle) . fromEnum

turnLeft :: Direction -> Direction
turnLeft = turn 1

turnBack :: Direction -> Direction
turnBack = turn 2

findRoutes :: Route -> [Path] -> [Route]
findRoutes route@(p:_) paths
  | next == goal = [route]
  | otherwise = do
      path <- [(next, dir), (next, turnLeft dir)]
      guard $ path `elem` paths
      findRoutes (path:route) $ paths \\ [path, flipPath path]
  where next = endPoint p
        (_, dir) = p

origin :: Path
origin = ((-1, 0), East)

main :: IO ()
main = do
  print $ length $ findRoutes [origin] allPaths
