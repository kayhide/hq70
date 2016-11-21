data Tap = OpenTap | DoubleTap Tap Tap | TripleTap Tap Tap Tap deriving (Eq, Ord, Show)

fit :: Int -> [Tap]
fit 1 = [OpenTap]
fit n = doubles ++ triples
  where doubles = do
          i <- [1..(n `div` 2)]
          DoubleTap <$> fit i <*> fit (n - i)
        triples = do
          i <- [1..(n `div` 3)]
          j <- [1..((n - i) ` div` 2)]
          TripleTap <$> fit i <*> fit j <*> fit (n - i - j)

main :: IO ()
main = print $ length $ fit 20
