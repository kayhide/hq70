{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Debug.Trace

type Position = (Int, Int)
type Size = (Int, Int)

data Motion = East | North | West | South deriving (Eq, Show, Enum)
data Board = Board Position Position deriving (Eq, Show)

type Path = [Motion]
type PathAndBoard = (Path, Board)

size = (3, 2)
-- size = (10, 10)
width = fst size
height = snd size

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

solve :: [Board] -> (Board -> Bool) -> [PathAndBoard] -> PathAndBoard
solve cache pred (head@(path, board):tail)
  | pred board = head
  | otherwise = solve cache' pred $ tail ++ next'
  where next' = catMaybes $ do
          mo <- enumFrom East
          return $ do
            board' <- moveVoid board mo
            guard $ notElem board' cache
            return ((mo:path), board')
        cache' = fmap snd next' ++ cache

main' :: IO ()
main' = print $ length $ fst $ solve [startBoard] isGoaled [([], startBoard)]


next :: PathAndBoard -> [PathAndBoard]
next (path, board) = catMaybes $ do
  mo <- enumFrom East
  return $ do
    board' <- moveVoid board mo
    return ((mo:path), board')

queue :: [PathAndBoard]
queue = ([], startBoard) : concat (fmap next queue)

main'' :: IO ()
main'' = print $ length $ fst $ fromJust $ find (isGoaled . snd) queue

type Cache = [Board]

-- |
-- >>> runState (nextWithCache ([], startBoard)) []
-- ([([North],Board (0,0) (2,0)),([West],Board (0,0) (1,1))],[Board (0,0) (2,0),Board (0,0) (1,1)])
nextWithCache :: PathAndBoard -> State Cache [PathAndBoard]
nextWithCache (path, board) = do
  cache <- trace "getting cache" get
  -- cache <- get
  let next' = catMaybes $! do
        mo <- enumFrom East
        return $ do
          board' <- moveVoid board mo
          guard $ trace "referencing cache" $ notElem board' cache
          return ((mo:path), board')
  -- modify' $ trace "modifing cache" ((snd <$> next')++)
  modify' ((snd <$> next')++)
  return next'

queueWithCache :: State Cache [PathAndBoard]
queueWithCache = do
  queue' <- queueWithCache
  next' <- sequence $ fmap nextWithCache queue'
  return $ ([], startBoard) : concat next'

find' = do
  queue' <- queueWithCache
  return $ fromJust $ find (isGoaled . snd) queue'

main = print $ length $ fst $ evalState find' []


caches :: [PathAndBoard] ->[[Board]]
caches queue = [] : scanl (flip (:)) [] (snd <$> queue)

paths :: [Path]
paths = tail $ concat $ iterate ((:) <$> motions <*>) [[]]
  where motions = enumFrom East
