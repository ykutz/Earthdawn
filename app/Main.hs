module Main where
import Control.Monad.Random.Strict
import System.Random
import Lib
import ExactCalc
import Data.List
import Utilities

main :: IO ()
main = do putStrLn "Start"
          let qs = calcQuantilesWithoutKarma quantiles
              qsK = calcQuantilesWithKarma quantiles
          putStrLn "Without Karma:"
          mapM_ (putStrLn . showQuantiles) qs
          putStrLn ""
          putStrLn "With Karma:"
          mapM_ (putStrLn . showQuantiles) qsK
          putStrLn "End"
          v <- getChar
          return ()


-- quantiles = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
quantiles = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55,
             0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95]

showQuantiles :: Show a => [a] -> String
showQuantiles qs = let qs' = map show qs
                       in concatWithString ";" qs'
