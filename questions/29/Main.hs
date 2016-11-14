{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Function

type Resistance = Double

goldenRatio :: Floating a => a
goldenRatio = (1 + sqrt 5) / 2

resistances :: [Resistance] -> [Resistance]
resistances [x] = [x]
resistances (x:xs) =
  fmap (+x) (resistances xs) ++ fmap (\y -> 1/(1/x + 1/y)) (resistances xs)

closest :: Resistance
closest = minimumBy (compare `on` (abs.(subtract goldenRatio))) $ resistances $ replicate 10 1.0

main :: IO ()
main = print $ fromIntegral (round (closest * 10^10)) / 10^10
