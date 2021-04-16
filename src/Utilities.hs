module Utilities where

concatWithString :: [a] -> [[a]] -> [a]
concatWithString string [] = []
concatWithString string (x:xs) = let xs' = map (string ++) xs
                                     in foldl (++) x xs'

mapWithIndex = mapWithIndexT 0
               where
                 mapWithIndexT i f [] = []
                 mapWithIndexT i f (x:xs) = f i x:mapWithIndexT (i+1) f xs


getMany :: [Int] -> [a] -> [a]
getMany = getManyT 0
          where
            getManyT _ [] _ = []
            getManyT _  _ [] = []
            getManyT i (k:ks) (l:ls)
              | i == k = l:getManyT (i+1) ks ls
              | otherwise = getManyT (i+1) (k:ks) ls

splitAtLast _ [] = ([], [])
splitAtLast f ls = let g [] ls = (ls, [])
                       g (x:xs) ls
                         | f x = (reverse (x:xs), ls)
                         | otherwise = g xs (x:ls)
                       in g (reverse ls) []

count :: Num a => (t -> Bool) -> [t] -> a
count = countT 0
        where
          countT c f [] = c
          countT c f (x:xs)
            | f x = countT (c+1) f xs
            | otherwise = countT c f xs
