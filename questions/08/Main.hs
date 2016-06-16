{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid

type Pos = (Int, Int)
type Route = [Pos]

class Monoid r => Routable r where
    runit :: Route -> r

instance Routable [Route] where
    runit route = [route]

instance Routable (Sum Int) where
    runit route = Sum 1

mroutes :: Routable r => Int -> Route -> r
mroutes 0 route = runit route
mroutes i route = mconcat $ map (mroutes (i - 1)) $ map (:route) $ nextPoss route

nextPoss :: Route -> [Pos]
nextPoss route@((x,y):_) = filter (not . (`elem` route)) candidates
    where
        candidates = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- patterns :: Int -> Route -> [Route]
-- patterns 0 route = [route]
-- patterns i route = mconcat $ map (patterns (i - 1)) $ map (:route) $ nextPoss route
patterns = mroutes :: Int -> Route -> [Route]

-- count :: Int -> Route -> Sum Int
-- count 0 _ = Sum 1
-- count i route = mconcat $ map (count (i - 1)) $ map (:route) $ nextPoss route
count = mroutes :: Int -> Route -> Sum Int


main :: IO ()
-- main = do
--     putStrLn $ show $ length $ patterns 12 [(0, 0)]
main = do
    putStrLn $ show $ count 12 [(0, 0)]
