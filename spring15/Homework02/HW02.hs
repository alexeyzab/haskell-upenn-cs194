{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches actual guess = countTruths 0 $ zipWith (==) actual guess

-- Counts the number of True Bools in a list
countTruths :: Int -> [Bool] -> Int
countTruths acc [] = acc
countTruths acc (x:xs)
  | x == False = countTruths acc xs
  | otherwise = countTruths (acc + 1) xs

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map counter colors
  where
    counter color = length $ filter (color ==) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum $ zipWith min (countColors actual) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = Move guess (exactMatches actual guess) (nonExactMatches actual guess)

-- Count number of non-exact matches between the actual code and the guess
nonExactMatches :: Code -> Code -> Int
nonExactMatches actual guess = (matches actual guess) - (exactMatches actual guess)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code = exactMatchesEql move code && nonExactMatchesEql move code

-- Checking if the exactMatches of the Move and the code are equal
exactMatchesEql :: Move -> Code -> Bool
exactMatchesEql move code = getExactMatches move == exactMatches (getCode move) code

-- Checking if the nonExactMatches of the Move and the code are equal
nonExactMatchesEql :: Move -> Code -> Bool
nonExactMatchesEql move code = getNonExactMatches move == nonExactMatches (getCode move) code

-- Getting the number of exact matches out of constructor
getExactMatches :: Move -> Int
getExactMatches (Move _ exactMatchesInt _) = exactMatchesInt

-- Getting the number of non-exact matches out of constructor
getNonExactMatches :: Move -> Int
getNonExactMatches (Move _ _ nonExactMatchesInt) = nonExactMatchesInt

-- Get the code out of a Move
getCode :: Move -> Code
getCode (Move code _ _) = code

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes _ [] = []
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes codeLength = concatMap addToFront $ allCodes (codeLength - 1)

-- Adds the code to the front
addToFront :: Code -> [Code]
addToFront code = map (code ++) (allCodes 1)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = map (getMove code) (filteredCodes code)

makeGuess :: Code -> Code
makeGuess [] = []
makeGuess (x:xs) = [Red] ++ makeGuess xs

listOfCodes :: Code -> [Code]
listOfCodes code = allCodes (length code)

filteredCodes :: Code -> [Code]
filteredCodes code = filterCodes (getMove code (makeGuess code)) $ listOfCodes code

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
