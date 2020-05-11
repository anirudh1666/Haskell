inRange :: Int -> Int -> [Int] -> [Int]
inRange _ _ [] = []
inRange x y zs = 
    let lower = filter (>=x) zs
        upper = filter (<=y) zs
    in lower ++ upper

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives xs = length $ filter (>0) xs

myLength :: [Int] -> Int
myLength [] = 0
myLength xs = foldr (+) 0 $ map foo xs
    where foo x = 1

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myLength' :: [Int] -> Int
myLength' [] = 0
myLength' xs = foldr (\_ acc -> acc + 1) 0 xs
