data Tap = OpenTap | DoubleTap Tap Tap | TripleTap Tap Tap Tap deriving (Eq, Ord, Show)

requestedCount :: Int
requestedCount = 4

countOpenTap :: Tap -> Int
countOpenTap OpenTap = 1
countOpenTap (DoubleTap x y) = (countOpenTap x) + (countOpenTap y)
countOpenTap (TripleTap x y z) = (countOpenTap x) + (countOpenTap y) + (countOpenTap z)


taps :: Int -> [Tap]
taps 0 = [OpenTap]
taps n = OpenTap : (DoubleTap <$> taps (n - 1) <*> taps (n - 1))
       ++ (TripleTap <$> taps (n - 1) <*> taps (n - 1) <*> taps (n - 1))

doubles :: Int -> [Tap]
doubles 0 = [DoubleTap OpenTap OpenTap]
doubles n = (DoubleTap OpenTap <$> both)
            ++ (DoubleTap <$> doubles' <*> both)
            ++ (DoubleTap <$> triples' <*> triples')
  where doubles' = doubles (n - 1)
        triples' = triples (n - 1)
        both = doubles' ++ triples'

triples :: Int -> [Tap]
triples 0 = [TripleTap OpenTap OpenTap OpenTap]
triples n = (TripleTap OpenTap OpenTap <$> both)
            ++ (TripleTap OpenTap <$> doubles' <*> both)
            ++ (TripleTap OpenTap <$> triples' <*> triples')
            ++ (TripleTap <$> doubles' <*> doubles' <*> both)
            ++ (TripleTap <$> doubles' <*> triples' <*> triples')
            ++ (TripleTap <$> triples' <*> triples' <*> triples')
  where doubles' = doubles (n - 1)
        triples' = triples (n - 1)
        both = doubles' ++ triples'

main :: IO ()
main = do
    putStrLn "answer"
