module Coursework where

{-
  Your task is to design a datatype that represents the mathematical concept of a (finite) set of elements (of the same type).
  We have provided you with an interface (do not change this!) but you will need to design the datatype and also 
  support the required functions over sets.
  Any functions you write should maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a list
  (as in the example below). Alternatively, one could use an algebraic data type,
  wrap a binary search tree, or even use a self-balancing binary search tree.
  Extra marks will be awarded for efficient implementations (a self-balancing tree will be
  more efficient than a linked list for example).

  You are NOT allowed to import anything from the standard library or other libraries.
  Your edit of this file should be completely self-contained.

  DO NOT change the type signatures of the functions below: if you do,
  we will not be able to test them and you will get 0% for that part. While sets are unordered collections,
  we have included the Ord constraint on most signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Please include everything in this file.
-}

{-
   PART 1.
   You need to define a Set datatype. Below is an example which uses lists internally.
   It is here as a guide, but also to stop ghci complaining when you load the file.
   Free free to change it.
-}

-- you may change this to your own data type
newtype Set a = Set { unSet :: [a] } deriving (Show)

{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.  
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Set a -> [a]            -- fix function so that you dont need to add Ord a type constraint
toList (Set {unSet = xs}) = xs

quickSort :: (Ord a) => [a] -> [a]          
quickSort [] = []
quickSort (x:xs) = 
   let listOne = quickSort [ y | y <- xs, y <= x]
       listTwo = quickSort [ z | z <- xs, z > x] 
   in listOne ++ [x] ++ listTwo

-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: (Ord a) => [a] -> Set a
fromList xs = Set {unSet = (quickSort (removeDuplicate xs))}

removeDuplicate :: (Eq a) => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (x:xs) = if x `elem` xs then removeDuplicate xs else x:(removeDuplicate xs) 



{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  s1 == s2 = True
  s1 /= s2 = False


-- the empty set
empty :: Set a
empty = Set {unSet = []}


-- Set with one element
singleton :: a -> Set a
singleton x = Set {unSet=[x]}


-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a 
insert x (Set {unSet=xs}) = Set {unSet = quickSort (removeDuplicate (x:xs))}


-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union (Set {unSet = xs}) (Set {unSet = ys}) = Set {unSet = quickSort (removeDuplicate (xs ++ ys))}


-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set {unSet = (x:xs)}) (Set {unSet = ys}) 
   | x `elem` ys   = Set {unSet = quickSort (x:(intersectionHelper (Set {unSet = xs}) (Set {unSet = ys})))}
   | otherwise     = Set {unSet = quickSort (intersectionHelper (Set {unSet = xs}) (Set {unSet = ys}))}

intersectionHelper :: (Eq a) => Set a -> Set a -> [a]
intersectionHelper (Set {unSet = []}) _ = []
intersectionHelper (Set {unSet = (x:xs)}) (Set {unSet = ys}) 
   | x `elem` ys  = (x:(intersectionHelper (Set {unSet = xs}) (Set {unSet = ys})))
   | otherwise    = (intersectionHelper (Set {unSet = xs}) (Set {unSet = ys}))


-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set {unSet = (x:xs)}) (Set {unSet = ys}) 
   | not (x `elem` ys) = Set {unSet = quickSort (x:(differenceHelper (Set {unSet = xs}) (Set {unSet = ys})))}
   | otherwise         = Set {unSet = quickSort ((differenceHelper (Set {unSet = xs}) (Set {unSet = ys})))}

-- helper function works on lists so you can concat it together and output it to difference which formats it into type Set a
differenceHelper :: (Eq a) => Set a -> Set a -> [a]
differenceHelper (Set {unSet = []}) _ = []
differenceHelper (Set {unSet = (x:xs)}) (Set {unSet = ys})
   | not (x `elem` ys) = x:(differenceHelper (Set {unSet = xs}) (Set {unSet = ys}))
   | otherwise         = differenceHelper (Set {unSet = xs}) (Set {unSet = ys})


-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member x (Set {unSet = xs}) = x `elem` xs

 
-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality (Set {unSet = xs}) = foldl (\acc _ -> acc + 1) 0 xs


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f (Set {unSet = xs}) = (Set {unSet = (mapHelper f xs)})

mapHelper :: (a -> b) -> [a] -> [b]
mapHelper f xs = reverseList (foldl (\acc x -> (f x) : acc) [] xs)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList xs = (last xs):(reverseList (init xs))

setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f (Set {unSet = xs}) acc = foldr (\acc x -> f acc x) acc xs


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)                          
powerSet (Set {unSet = xs}) = undefined

-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian (Set {unSet = xs}) (Set {unSet = ys}) = Set {unSet = (cartesianHelper xs ys)}

cartesianHelper :: [a] -> [b] -> [(a,b)]
cartesianHelper [] _ = []
cartesianHelper (x:xs) ys = (tupleMap x ys) ++ (cartesianHelper xs ys) where
   tupleMap x ys = [ (x,z) | z <- ys]

   
-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition f (Set {unSet = []}) = ((Set {unSet = []}), (Set {unSet = []}))
partition predicate (Set {unSet = xs}) = ((Set {unSet = [ x | x <- xs, predicate x == True]}), (Set {unSet = [ x | x <- xs, predicate x == False]})) 

{-
   On Marking:
   Be careful! This coursework will be marked using QuickCheck, against Haskell's own
   Data.Set implementation. Each function will be tested for multiple properties.
   Even one failing test means 0 marks for that function.

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough for a
   passing mark of 40%.

   The maximum mark for those who use lists, as in the example above, is 70%. To achieve
   a higher grade than is, one must write a more efficient implementation.
   100% is reserved for those brave few who write their own self-balancing binary tree.
-}
