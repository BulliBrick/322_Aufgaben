--Aufgaben

import Data.Char

--funktionen
summe :: Integer -> Integer
summe 0 = 0
summe x
    | x < 0 = error "x muss größer gleich 0 sein"
    | otherwise = x + summe (x-1)

equals :: Integer -> Integer -> Bool
equals a b
    | a == b = True
    | otherwise = False

bigger :: Integer -> Integer -> Integer
bigger a b
    | a > b = a
    | otherwise = b

--rekursion

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



--demo lists
ten :: [Int]
ten = [1..10]

cube :: [Int]
cube = [x*x*x | x <- [1..10]]

sumCube :: Int
sumCube = sum cube

-- >>> cube [12]
--lists
rangeList :: Int -> Int -> [Int]
rangeList a b = [a..b]


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

--map filter
toUpperString :: String -> String
toUpperString [] = []
toUpperString (x:xs) = toUpper x : toUpperString xs

toLowerString :: String -> String
toLowerString [] = []
toLowerString (x:xs) = toLower x : toLowerString xs


isLowerCase :: Char -> Bool
isLowerCase c = isLower c
stringToLowerCaseBools :: String -> [Bool]
stringToLowerCaseBools str = map isLowerCase str

allLowerCase :: String -> Bool
allLowerCase str = and (stringToLowerCaseBools str)


minList :: [Int] -> [Int] -> [Int]
minList xs ys = zipWith min xs ys


-- reduction

productList :: [Int] -> Int
productList = foldl (*) 1

productEvenList :: [Int] -> Int
productEvenList = foldl (*) 1 . filter even

maxList :: [Int] -> Int
maxList = foldl1 max

anyTrue :: [Bool] -> Bool
anyTrue = foldl (\acc x -> acc || x) False