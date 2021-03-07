-- List comprehensions and data types

-- list comprehensions

list1 = [3*n + 1 | n <- [0..10],even n, n > 3]

list2 = [3*n + m | n <-[0..5], m <- [100,200]]



-- style 

fibs :: [Int]
fibs = 1 : 1 : [i + j | (i,j) <- zip fibs (tail fibs)]


ordered :: Ord a => [a] -> Bool 
ordered xs = and [i <= j | (i,j) <- zip xs (tail xs)]



-- function composition
-- (f . g) = f (g x)

smallsPlusSix :: [Int] -> [Int]
smallsPlusSix xs = map (6+) (filter (<=4) xs)


-- smallsredone :: [Int] -> [Int]
-- smallsredone = map (6+) . (filter (<=4) xs)






reverseShorts :: [String] -> [String]
reverseShorts = map reverse . filter ((<=5) . length)



-- data types 

data TwoStrings = TwoStr String String 

frompair :: (String,String) -> TwoStrings
frompair (s1,s2) = (TwoStr s1 s2)



data Term = Variable String 
         | Lambda String Term 
         | Apply Term Term





-- 4a excercises 

--function to double every element in a list

doublee :: [Int] -> [Int]
doublee xs = [i*2 | i <- xs]

-- function to remove even items from a list of integers and doubles the odds
odds :: [Int] -> [Int]
odds xs = [i*2 | i <- xs , even i]



--function to remove <5 leng from lits of strings

lessthan5 :: [String] -> [String]
lessthan5 xs = [i | i <- xs, (length i < 5)]



squarepos :: [Int] -> [Int]
squarepos xs = [ i*i | i <- xs , i>0]



--sum of all odd length lists 
sumodd :: [[Int]] -> [Int]
sumodd xs = [sum i | i <- xs ,  even (length i) ]


-- remove occurances from a list 
remo :: Eq a  => [a] -> a  -> [a]
remo xs a = [ i | i <- xs , i /= a]



-- remove all elements of second list from first list 
remoall :: Eq a => [a] -> [a] -> [a]
remoall xs yn = [i | i <- xs , not (i `elem` yn) ]


-- every other element of a list 
everyother :: [t] -> [t]
everyother xs = [fst i  | i <- zip (xs) [1..(length xs)],not( even (snd i))]



-- function to take two lists and return the positions of elements that are equal 
same :: Eq a => [a] -> [a] -> [(Int)]
same xs yz = [ snd i | i <- zip (zipWith (==) xs yz) ([1..length xs]), fst i == True]



-- function to generate cartesian product of two lists 
pairs :: [a] -> [a] -> [(a,a)]
pairs xs ys = [(x,y) | x <-xs, y <- ys]



-- generate all pythagorean triples up to n 
pythag :: Int -> [(Int,Int,Int)]
pythag n = [ (x,y,x) | x <- [1..n], y <- [1..n] , z <- [1..n], (((x**2) + (y**2)) == (x**2))]
