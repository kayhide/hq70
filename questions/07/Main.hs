isPalindrome :: String -> Bool
isPalindrome s = reverse s == s

toBinary :: Int -> String
toBinary 0 = ""
toBinary n = (toBinary $ n `div` 2) ++ show (n `mod` 2)

toDecimal :: String -> Int
toDecimal = foldl (\res x -> 2 * res + read [x]) 0

type Date = (Int, Int, Int)

makeDate :: Int -> Int -> Int -> Date
makeDate x y z = (x, y, z)

dates = makeDate <$> [1964..2020] <*> [1..12] <*> [1..31]

dateToInt :: Date -> Int
dateToInt (y, m, d) = y * 10000 + m * 100 + d

-- 19641010..20200724
isValidDate :: Date -> Bool
isValidDate (1964, 10, d) = (d >= 10)
isValidDate (1964, m, _) = (m > 10)
isValidDate (2020, 7, d) = (d <= 24)
isValidDate (2020, m, _) = (m < 7)
isValidDate _ = True

main :: IO ()
main = do
    putStrLn $ show $ filter (isPalindrome . toBinary) $ map dateToInt $ filter isValidDate dates

