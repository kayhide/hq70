import Control.Monad

isPalindrome :: String -> Bool
isPalindrome s = reverse s == s

toBinary :: Int -> String
toBinary = foldl (\a e -> show (e `mod` 2) ++ a) "" . takeWhile (>0) . iterate (`div` 2)

toDecimal :: String -> Int
toDecimal = foldl (\a e -> 2 * a + read [e]) 0

type Date = (Int, Int, Int)

dateToInt :: Date -> Int
dateToInt (y, m, d) = y * 10000 + m * 100 + d

-- 19641010..20200724
isValidDate :: Date -> Bool
isValidDate (1964, 10, d) = (d >= 10)
isValidDate (1964, m, _) = (m > 10)
isValidDate (2020, 7, d) = (d <= 24)
isValidDate (2020, m, _) = (m < 7)
isValidDate _ = True

dates :: [Date]
dates = do
  date <- (,,) <$> [1964..2020] <*> [1..12] <*> [1..31]
  guard $ isValidDate date
  return date

main :: IO ()
main = do
     print $ filter (isPalindrome . toBinary) $ dateToInt <$> dates
