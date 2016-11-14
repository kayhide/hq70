{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Function
import Data.Ratio

type Resistance = Rational

goldenRatio :: Floating a => a
goldenRatio = (1 + sqrt 5) / 2

resistances :: [Resistance] -> [Resistance]
resistances [x] = [x]
resistances (x:xs) =
  fmap (+x) (resistances xs) ++ fmap (\y -> recip (recip x + recip y)) (resistances xs)

closest :: Resistance
closest = minimumBy compare' $ resistances $ replicate 10 1
  where compare' = compare `on` (abs.(subtract goldenRatio).fromRational)

main :: IO ()
main = do
  print closest
  print $ fromIntegral (round (closest * 10^10)) / 10^10
