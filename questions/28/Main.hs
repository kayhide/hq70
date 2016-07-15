import Data.List

type Area = Int
type MemberCount = Int
type Club = (Area, MemberCount)

allClubs :: [Club]
allClubs = [(11000, 40), (8000, 30), (400, 24), (800, 20), (900, 14),
            (1800, 16), (1000, 15), (7000, 40), (100, 10), (300, 12)]

memberCountLimit :: Int
memberCountLimit = 150


totalArea :: [Club] -> Int
totalArea clubs = (sum $ map fst clubs)

isValid :: [Club] -> Bool
isValid clubs = (sum $ map snd clubs) <= memberCountLimit


main :: IO ()
main = do
    print $ maximum $ fmap totalArea $ filter isValid $ subsequences allClubs
