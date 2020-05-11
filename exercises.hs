import Data.Char

halfEvens :: [Int] -> [Int]
halfEvens [] = []
halfEvens xs  
    | (head xs) `mod` 2 == 0 = ((head xs) `div` 2) : halfEvens (tail xs)
    | otherwise      = (head xs) : halfEvens (tail xs)


inRange :: Int -> Int -> [Int] -> [Int]
inRange upper lower [] = []
inRange upper lower xs = if ((head xs) >= lower && ((head xs) <= upper)) then (head xs) : inRange upper lower (tail xs) else inRange upper lower (tail xs) 

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives xs = if (head xs) >= 0 then 1 + (countPositives (tail xs)) else 0 + (countPositives (tail xs))

capitalised :: String -> String
capitalised "" = ""
capitalised xs = toUpper (head xs) : lower (tail xs)


lower :: String -> String
lower "" = ""
lower xs = toLower (head xs) : lower (tail xs)

title :: [String] -> [String]
title [] = []
title xs = capitalised (head xs) : helper (tail xs)

allLower :: String -> String
allLower xs = [toLower x | x <- xs]

helper :: [String] -> [String]
helper [] = []
helper xs = if length (head xs) > 3 then capitalised (head xs) : helper (tail xs) else allLower (head xs) : helper (tail xs)

isort :: Ord a => [a] -> [a]
isort [] = []
isort xs = insert (head xs) (isort (tail xs))


insert :: Ord a=> a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y then x:(y:ys) else y:(insert x ys)