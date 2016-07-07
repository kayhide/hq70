type State = (Int, Int)
type Sequence = [State]

nextStates :: State -> [State]
nextStates (x,y) = [(x + i, y - j) | i <- [1..4], j <- [1..4]]

isMet :: State -> Bool
isMet (x, y) = x == y

isPassed :: State -> Bool
isPassed (x, y) = x > y

go :: Sequence -> [Sequence]
go seq@(st:_)
    | isMet st = [seq]
    | isPassed st = []
    | otherwise = concat $ map go $ map (:seq) $ nextStates st

main :: IO ()
main = do
    putStrLn $ show $ length $ go [(0, 10)]
