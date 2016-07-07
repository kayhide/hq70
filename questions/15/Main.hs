type State = (Int, Int)
type Sequence = [State]

nextStates :: State -> [State]
nextStates (x,y) = do
    i <- [1..4]
    j <- [1..4]
    return (x + i, y - j)

isMet :: State -> Bool
isMet (x, y) = x == y

isPassed :: State -> Bool
isPassed (x, y) = x > y

go :: Sequence -> [Sequence]
go seq@(st:_)
    | isMet st = [seq]
    | isPassed st = []
    | otherwise = concatMap (go . (:seq)) $ nextStates st

main :: IO ()
main = do
    print $ length $ go [(0, 10)]
