module ExactCalc(calcQuantilesWithoutKarma,
                 calcQuantilesWithKarma) where
import Utilities
--import Debug.Trace
import Debug.NoTrace
import Data.List


newtype Dice = D Int deriving(Show, Eq, Ord)


probabilitySingle :: Dice -> Int -> Double
probabilitySingle d@(D n) x
  | x <= 0 = 0
  | x < n = 1 / fromIntegral n
  | x == n = 0
  | otherwise = (1 / fromIntegral n) * probabilitySingle d (x-n)

probability :: [Dice] -> Int -> Double
probability [] _ = 0
probability [d] x = probabilitySingle d x
probability (d:ds) x
  | x <= 0 = trace ("Prob for: " ++ show x) 0
  | otherwise = let ps = trace ("Prob for: " ++ show x) map (probabilitySingle d) [1..x-1]
                    psOfNumber = zip [1..x-1] ps
                    t = map f psOfNumber
                    f (y, p)
                      | p <= 0.00001 = 0
                      | otherwise = p * probability ds (x - y)
                    in sum t

quantiles = leftQuantilesT 0
                where
                  leftQuantilesT _ [] _ = []
                  leftQuantilesT c qs ((x,p):ps) -- ps hase to be large
                    = let p' = c + p
                          n = numberOfReachedQuantiles p' qs
                          in replicate n x ++ leftQuantilesT (c+p) (drop n qs) ps

numberOfReachedQuantiles p = count (<= p)

allLeftQuantiles :: [Dice] -> [Double] -> [Int]
allLeftQuantiles ds qs = let ls = map (\x -> (x, probability ds x)) [1..]
                             in quantiles qs (take 10000 ls)

allRightQuantiles :: [Dice] -> [Double] -> [Int]
allRightQuantiles ds qs = let ls = map (\x -> (x, probability ds x)) [1..]
                              in reverse $ quantiles qs (reverse $ take 1000 ls)

calcQuantilesWithoutKarma qs = map (\ds -> allLeftQuantiles ds (sort qs)) steps
calcQuantilesWithKarma qs = map (\ds -> allLeftQuantiles ds (sort qs)) stepsWithKarma

steps = [[D 4],[D 6],[D 8],[D 10],[D 12],[D 6, D 6],[D 8, D 6],[D 8, D 8],
         [D 10, D 8],[D 10, D 10],[D 12, D 10],[D 12, D 12],
         [D 12, D 6, D 6],[D 12, D 8, D 6],[D 12, D 8, D 8],[D 12, D 10, D 8],
         [D 20, D 6, D 6],[D 20, D 8, D 6],[D 20, D 8, D 8],[D 20, D 10, D 8],
         [D 20, D 10, D 10],[D 20, D 12, D 10],[D 20, D 12, D 12],
         [D 20, D 12, D 6, D 6],[D 20, D 12, D 8, D 6],[D 20, D 12, D 8, D 8],
         [D 20, D 12, D 10, D 8],[D 20, D 20, D 6, D 6],[D 20, D 20, D 8, D 6],
         [D 20, D 20, D 8, D 8],[D 20, D 20, D 10, D 8],[D 20, D 20, D 10, D 10],
         [D 20, D 20, D 12, D 12],[D 20, D 20, D 12, D 6, D 6],
         [D 20, D 20, D 12, D 8, D 6],[D 20, D 20, D 12, D 8, D 8],
         [D 20, D 20, D 12, D 10, D 8]]

stepsWithKarma = map (D 6:) steps
