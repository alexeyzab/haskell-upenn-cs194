-- Note: the solutions has been taken from
-- here: https://github.com/gabebw/haskell-upenn-cs194/blob/master/spring-2013/hw3/Golf.hs

module Golf where

import Data.List

-- This function takes a list, and then outputs a list of lists. First
-- inner list is the original list itself. Every next element contains
-- every nth element from the original list.  n being the position of an
-- inner list, starting with 2.
skips :: [a] -> [[a]]
skips xs = map (skipsCore xs) [1..(length xs)]

skipsCore :: [a] -> Int -> [a]
skipsCore [] n = []
skipsCore xs n = map snd $ filter dividesInto (elemsWithIndices xs)
    where
      dividesInto (x, _) = x `mod` n == 0

elemsWithIndices :: [a] -> [(Int, a)]
elemsWithIndices = zip [1..]


-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it.
--
-- > localMaxima [2,9,5,6,1] == [9,6]
-- > localMaxima [2,3,4,1,5] == [4]
-- > localMaxima [1,2,3,4,5] == []
--
-- We define localMaxima for a sequence of three separate elements and
-- then recursively use it for longer lists.
localMaxima :: [Integer] -> [Integer]

localMaxima (x:y:z:[]) = if y > x && y > z then [y] else []
localMaxima (x:y:z:xs) = localMaxima [x, y, z] ++ localMaxima (y:z:xs)
localMaxima _ = []

-- A function that takes as input a list of Integers between 0 and
-- 9 (inclusive), and outputs a vertical histogram showing how many of each
-- number were in the input list.
--
-- The solution here is taken from gabebw's repo
-- I've typed it in for the sake of understanding what's actually happening
-- First, he creates a special data type
data Bucket = Bucket {
            num :: Int
            , occurrences :: Int
            , maxOccurrences :: Int
            }

-- Then he sets the rules for comparing two buckets, based on the number
-- they represent.
instance Eq Bucket where
    (Bucket n1 _ _) == (Bucket n2 _ _) = n1 == n2

instance Ord Bucket where
    (Bucket n1 _ _) `compare` (Bucket n2 _ _) = n1 `compare` n2


histogram :: [Int] -> String

histogram [] = ""
histogram ns = intercalate "\n" .
    transpose .
    map (reverse . line) .
    fillInMissingBuckets .
    buckets $ ns

-- Given a list of numbers like [1, 1, 1, 5], create a list of Buckets.
buckets :: [Int] -> [Bucket]
buckets ns = map makeBucket grouped
    where
      makeBucket ngroup@(n:_) = Bucket n (length ngroup) maximumSize
      maximumSize = maximum $ map length grouped
      grouped = group (sort ns)

-- A given list of buckets may have missing buckets for some of the
-- numbers. For example, if the original list is [1, 1, 2], it only has
-- Buckets for 1 and 2. This would add Buckets with a count of 0 for the
-- numbers 3-10.
fillInMissingBuckets :: [Bucket] -> [Bucket]
fillInMissingBuckets bs@(b:_) = sort $ nub (bs ++ empty0to9)
    where
      empty0to9 = map (\n -> Bucket n 0 most) [0..9]
      most = maxOccurrences b

-- Given a Bucket, print out a horizontal graph of the occurrences of that
-- value. It will be transformed into a vertical version by `transpose` in
-- `histogram`.
line :: Bucket -> String
line (Bucket n count lineLength) = (show n) ++ "=" ++ stars ++ paddingSpaces
    where
      stars = replicate count '*'
      paddingSpaces = replicate (lineLength - count) ' '
