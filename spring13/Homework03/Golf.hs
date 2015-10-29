module Golf where

import Data.List

-- This function takes a list, and then outputs a list of lists. First
-- inner list is the original list itself. Every next element contains
-- every nth element from the original list.  n being the position of an
-- inner list, starting with 2.
skips :: [a] -> [[a]]
skips xs = map (skipsStr xs) [1..(length xs)]

skipsStr :: [a] -> Int -> [a]
skipsStr [] n = []
skipsStr xs n = map snd $ filter dividesInto (elemsWithIndices xs)
    where
      dividesInto (x, _) = x `mod` n == 0

elemsWithIndices :: [a] -> [(Int, a)]
elemsWithIndices = zip [1..]

localMaxima :: [Integer] -> [Integer]

localMaxima xs = xs
