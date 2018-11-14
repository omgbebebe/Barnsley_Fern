module Main where

import Graphics.Gloss
import Control.Arrow
import System.Random

dot :: Point -> Picture
dot (x,y) = translate x y $ color green $ circleSolid 0.01

scene :: [Integer] -> Picture
scene ps = scale 70 70 $ translate 0 (-5.0) $ pictures $ map dot points
  where points = take 30000 $ fern ps

nextPoint :: Integer -> Point -> Point
nextPoint p (x, y) | p <= 2    = ( 0.0, 0.16 * y)
                   | p <= 86   = ( 0.85 * x + 0.04 * y, -0.04 * x + 0.85 * y + 1.6)
                   | p <= 93   = ( 0.2  * x - 0.26 * y,  0.23 * x + 0.22 * y + 1.6)
                   | otherwise = (-0.15 * x + 0.28 * y,  0.26 * x + 0.24 * y + 0.44)

fern :: [Integer] -> [Point]
fern ps = scanl (\p prob -> nextPoint prob p) (0,0) ps

randomRsIO :: Random a => (a, a) -> IO [a]
randomRsIO range = getStdRandom $ split >>> first (randomRs range)

genProbs :: IO [Integer]
genProbs = randomRsIO (0, 100)

main :: IO ()
main = do
  probability <- genProbs
  display (InWindow "108 - Barnsley Fern" (400, 400) (10, 10)) black (scene probability)
