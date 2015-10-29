import Data.List

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits (-17) = []
toDigits n = map (read::String->Integer) (toStringList n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev (-17) = []
toDigitsRev n = reverse (toDigits n)

toStringList :: Integer -> [String]
toStringList n = map (:[]) (show n)

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther = reverse . doubleFromLeft . reverse

doubleFromLeft :: [Integer] -> [Integer]

doubleFromLeft [] = []
doubleFromLeft (x:y:xs) = x : 2 * y : doubleFromLeft xs

sumDigits :: [Integer] -> Integer

sumDigits xs = sumList (toDigits (toNewInteger xs))

sumList :: [Integer] -> Integer

sumList = foldr (+) 0

toNewInteger :: [Integer] -> Integer
toNewInteger xs = (read::String->Integer) (intercalate "" (map show xs))

validate :: Integer -> Bool

validate n
    | sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0 = True
    | otherwise = False
