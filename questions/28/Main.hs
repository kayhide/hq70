type Area = Int
type Count = Int

type Club = (Area, Count)

allClubs :: [Club]
allClubs = [(11000, 40), (8000, 30), (400, 24), (800, 20), (900, 14),
            (1800, 16), (1000, 15), (7000, 40), (100, 10), (300, 12)]

sumClubs :: [Club] -> (Area, Count)
sumClubs [] = (0, 0)
sumClubs ((area, count):xs) = (area + areas, count + counts)
    where (areas, counts) = sumClubs xs

isValid :: [Club] -> Bool
isValid clubs = totalCount <= 150
    where (_, totalCount) = sumClubs clubs

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = map (x:) (combinations xs) ++ (combinations xs)


main :: IO ()
main = do
    putStrLn $ show $ maximum $ fmap (fst . sumClubs) $ filter isValid $ combinations allClubs
