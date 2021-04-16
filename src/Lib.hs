module Lib
    ( calcQuantiles
    ) where
import Control.Monad.Random.Strict
import System.Random
import Data.List
import Utilities

-- Statistics
quantiles :: [Double] -> [Int] -> [Int]
quantiles qs unsorted
  = let sorted = sort unsorted
        n = fromIntegral $ length unsorted
        ks = map (floor . (*n)) (sort qs)
        in getMany ks sorted

-- Dice
newtype Dice = D Int deriving(Show, Eq, Ord)

throw :: RandomGen g => Dice -> Rand g Int
throw (D x) = do v <- getRandomR (1, x)
                 if v == x then
                    do v2 <- throw (D x)
                       return $ v + v2
                  else return v

throwMany ds = do vs <- mapM throw ds
                  return $ sum vs

throwManyOften :: RandomGen g => Int -> [Dice] -> Rand g [Int]
throwManyOften 0 ds = return []
throwManyOften n ds = do v <- throwMany ds
                         vs <- throwManyOften (n-1) ds
                         return $ v:vs

calcQuantiles :: RandomGen g => [Double] -> Rand g [[Int]]
calcQuantiles qs = do results <- mapM (throwManyOften 1000000) steps
                      return $ map (quantiles qs) results

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
