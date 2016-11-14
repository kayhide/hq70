{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Debug.Trace

type Position = (Int, Int)
type Size = (Int, Int)

data Motion = East | North | West | South deriving (Eq, Show, Enum)
data Board = Board Position Position deriving (Eq, Ord, Show)

type Path = [Motion]
type PathAndBoard = (Path, Board)

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

type Cache = Set Board

nextWithCache :: PathAndBoard -> State Cache [PathAndBoard]
nextWithCache (path, board) = do
  cache <- get
  let next' = catMaybes $ do
        mo <- enumFrom East
        return $ do
          board' <- moveVoid board mo
          guard $ Set.notMember board' cache
          return ((mo:path), board')
  put $ foldr Set.insert cache $ fmap snd next'
  return next'

queueWithCache :: [PathAndBoard] -> State Cache [PathAndBoard]
queueWithCache current = do
  nexts <- sequence $ fmap nextWithCache current
  let next = concat nexts
  (next ++) <$> queueWithCache next

main = print $ length $ fst $ evalState find' Set.empty
  where find' = fromJust . find (isGoaled . snd) <$> queueWithCache [([], startBoard)]
