module Main where

import           ClassyPrelude

import Control.Lens

count :: Int
count = 6

data Path = Forward | Backward
  deriving (Show, Eq)

type State = [[Maybe Path]]

type Pos = (Int, Int)


initialState :: Int -> State
initialState n =
  replicate n $ replicate (n + 1) Nothing

formatState :: State -> Text
formatState state = unlines $ format' state
  where
    format' :: State -> [Text]
    format' state' = head' state' : tail' state'

    head' :: State -> Text
    head' state' = maybe "" (intercalate "  " . fmap toChar') $ traverse headMay state'

    tail' :: State -> [Text]
    tail' state' = maybe [] format' (traverse tailMay state')

    toChar' :: Maybe Path -> Text
    toChar' Nothing = "--"
    toChar' (Just Forward) = "->"
    toChar' (Just Backward) = "<-"

go :: State -> Pos -> [State]
go [] _ = [[]]
go (xs : xss) (i, j) = do
  let n = length xs - 1
  i' <- [i .. n]
  j' <- delete i' [j .. n]
  guard $ i == i' || j == j' || (not $ overlaps (i, i') (j, j'))
  guard $ xss /= [] || i' == n || j' == n
  ((xs & ix i' .~ Just Forward & ix j' .~ Just Backward) :) <$> go xss (i', j')

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (i, j) (i', j') = ((max i' j') - (min i j)) * ((min i' j') - (max i j)) < 0

main :: IO ()
main = do
  -- traverse_ (putStr . formatState) $ take 10 $ go (initialState count) (0, 0)
  print $ length $ go (initialState count) (0, 0)


