module Main where

import           ClassyPrelude

import           Control.Lens
import           Data.Semigroup

data Size = Size Int Int
  deriving (Eq, Show)

data Orientation = Horizontal | Vertical
  deriving (Eq, Ord, Show)

type Layout = [[Maybe Tatami]]

type Pos = (Int, Int)
type Tatami = (Orientation, Pos)
type Room = [Tatami]


formatLayout :: Layout -> Text
formatLayout xss = unlines $ concatMap toChar' <$> xss
  where
    toChar' :: Maybe Tatami -> Text
    toChar' Nothing                = "."
    toChar' (Just (Horizontal, _)) = "-"
    toChar' (Just (Vertical, _))   = "|"

candidates :: Size -> [Tatami]
candidates (Size h w) =
  do
    j <- [0 .. h - 1]
    i <- [0 .. w - 2]
    pure (Horizontal, (i, j))
  <>
  do
    j <- [0 .. h - 2]
    i <- [0 .. w - 1]
    pure (Vertical, (i, j))

go :: [Tatami] -> [Room]
go []       = [[]]
go (t : ts) = ((t :) <$> go (reject t ts)) <> go ts


reject :: Tatami -> [Tatami] -> [Tatami]
reject t = filter (not . overlaps t)

flatten :: Tatami -> [Pos]
flatten (Horizontal, (i, j)) = [(i, j), (i + 1, j)]
flatten (Vertical, (i, j))   = [(i, j), (i, j + 1)]

overlaps :: Tatami -> Tatami -> Bool
overlaps t t' = not . null $ do
  ps <- flatten t
  ps' <- flatten t'
  guard $ ps == ps'
  pure ()

toLayout :: [Tatami] -> Layout
toLayout ts = foldl' put' (replicate h (replicate w Nothing)) ts
  where
    (w, h) = ((+ 1) . getMax *** (+ 1) . getMax) . mconcat $ (Max *** Max) <$> concatMap flatten ts

    put' :: Layout -> Tatami -> Layout
    put' layout t =
      foldl' (\l (i, j) -> l & ix j . ix i .~ Just t) layout $ flatten t


isValid :: Size -> Room -> Bool
isValid size@(Size h w) ts =
  (h * w `div` 2 == length ts) &&
  all (not . isDistinct) (squares (toLayout ts))
  where
    isDistinct :: [Maybe Tatami] -> Bool
    isDistinct = all ((== 1) . length) . group . sort

squares :: [[a]] -> [[a]]
squares xss =
  fmap (^.. both . both) . join
  . uncurry (zipWith zip) . (id &&& drop 1)
  $ uncurry zip . (id &&& drop 1) <$> xss


main :: IO ()
main = do
  -- putStrLn $ formatLayout $
  --   [replicate 4 (Just Horizontal)] <>
  --   replicate 2 [Just Vertical, Just Horizontal, Just Horizontal, Just Vertical]
  -- traverse_ print' $ pure <$> candidates size
  -- run $ Size 3 4
  run $ Size 4 7
  run $ Size 5 6
  where
    run :: Size -> IO ()
    run size = do
      let answers = filter (isValid size) $ go $ candidates size
      traverse_ print' $ answers

    print' :: Room -> IO ()
    print' ps = do
      putStr . formatLayout . toLayout $ ps
      print $ length ps
      putStrLn ""
