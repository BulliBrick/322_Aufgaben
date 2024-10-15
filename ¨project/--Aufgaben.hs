--Aufgaben

import Data.Char

summe :: Integer -> Integer
summe 0 = 0
summe x
    | x < 0 = error "x muss größer gleich 0 sein"
    | otherwise = x + summe (x-1)

fakultaet :: Integer -> Integer
fakultaet 0 = 0
fakultaet 1 = 1
fakultaet x
    | x < 0 = error "x muss größer gleich 0 sein"
    | otherwise = x * fakultaet (x-1)

fibonacci :: Integer -> Integer
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci x
    | x < 0 = error "x muss größer gleich 0 sein"
    | otherwise = fibonacci (x-1) + fibonacci (x-2)



ggt :: Integer -> Integer -> Integer
ggt 0 b = b
ggt a 0 = a
ggt a b
    | a < 0 = error "a muss größer gleich 0 sein"
    | b < 0 = error "b muss größer gleich 0 sein"
    | a > b = ggt (a-b) b
    | a < b = ggt a (b-a)
    | a == b = a



hoch :: Integer -> Integer -> Integer
hoch 0 _ = 0
hoch _ 0 = 1
hoch a b
    | b < 0 = error "b muss größer gleich 0 sein"
    | otherwise = a * hoch a (b-1)


division :: Int -> Double
division 1 = 1.0 
division a
    | a == 0 = error "a darf nicht 0 sein"
    | otherwise = 1 / fromIntegral a + division (a-1)

-- >>> division 5
-- 2.283333333333333





ten :: [Int]
ten = [1..10]

cube :: [Int]
cube = [x*x*x | x <- [1..10]]

sumCube :: Int
sumCube = sum cube

-- >>> cube [12]

rangeList :: Int -> Int -> [Int]
rangeList a b = [a..b]

-- >>> rangeList 1 10

palindromList :: Eq a => [a] -> Bool
palindromList x = reverse x == x

dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

firstWord :: String -> String
firstWord [] = []
firstWord (x:xs)
    | x == ' ' = []
    | otherwise = x : firstWord xs


toUpperString :: String -> String
toUpperString [] = []
toUpperString (x:xs) = toUpper x : toUpperString xs

-- >>> toUpperString "hallo"
-- "HALLO"
toLowerString :: String -> String
toLowerString [] = []
toLowerString (x:xs) = toLower x : toLowerString xs

-- >>> toLowerString "HALLO"
-- "hallo"

-- >>> firstWord "Hallo Welt"
-- "Hallo"

-- >>> dupli [1,2,3]
-- [1,1,2,2,3,3]

-- >>> palindromList [1,2,3,2,1]
-- True

-- >>> rangeList 1 10
-- [1,2,3,4,5,6,7,8,9,10]



checkUpper :: string -> [Bool]
checkUpper x = []

-- >>> checkUpper "Hallo"
-- []
