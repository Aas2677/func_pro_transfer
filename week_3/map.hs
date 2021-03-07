-- Lists and higher-order functions
 

addone :: [Int] -> [Int]
addone []  = []
addone (x:xs) = (x+1) : addone xs  



squarethemall :: [Int] -> [Int]
squarethemall [] = [] 
squarethemall (x:xs) = (x^2): squarethemall xs



-- generalising using a function on a collection 

map1 :: (a ->b) -> [a] -> [b]
map1 f [] = [] 
map1 f (x:xs) = f x : map1 f xs 


addone1 :: [Int] -> [Int]
addone1 = map1 (+1)


squarethemall1 :: [Int] -> [Int]
squarethemall1 = map1 (^2)


-- going crazy with map1 and recursion 

counter :: [Int]
counter = 0 : map1(+1) counter 




indiv :: Int -> Int -> Bool 
indiv x y = (mod y x) /= 0 


--filtering to find primes 

sieve :: [Int] -> [Int]
sieve [] = [] 
sieve (x:xs) = x: sieve (filter(indiv x) xs)


primes :: [Int]
primes = sieve [2..1000]

plus :: (Int,Int) -> Int 
plus (x,y) = x+y  

addlists ::[Int] -> [Int] -> [Int]
addlists xs ys = map plus (zip xs ys)

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)





-------3A Activities.

-- function to double every number in a list:
 
dub :: Int -> Int
dub x = 2*x 

recdub :: [Int] -> [Int]
recdub [] = []
recdub (x:xs) = (dub x):(recdub xs)

mapdub :: [Int] -> [Int]
mapdub x = map dub x


--- function to remove even numbers from a list 

odder :: [Int] -> [Int]
odder [] = [] 
odder (x:xs)
 | odd x = (x:odder xs)
 | otherwise = (odder xs)


odder2 :: [Int] -> [Int]
odder2 = filter odd 


-- function to remove evens and double every odd number 
doubleodds :: [Int] -> [Int]
doubleodds [] = []
doubleodds x = map (\i -> i*2) (odder2 x)



--- Exercise 2:
-- remove strings longer than 5 characters from list of strings 

shorts :: [String] -> [String]
shorts x = filter s x
    where
        s :: [Char] -> Bool
        s word = length word <= 5

-- function to take positive integers in a list and square them

sqp :: [Int] -> [Int]
sqp x = map (\i -> i*i) (filter fil x)
    where
        fil :: Int -> Bool
        fil n = n > 0

-- function that takes a list of integer lists. returns for each odd-length list
-- its sum 
oddlengthsums :: [[Int]] -> [Int]
oddlengthsums x = map (\i -> sum i) (filter oop x)
      where 
          oop :: [Int] -> Bool
          oop x =  odd (length x)


--- ex 3 
-- a nah  

--b 
numbered :: [a] -> [(Int,a)]
numbered x = let n = length x in zip [0..n] x

--c 
everyother :: [a] -> [a]
everyother x =  map snd (filter oddx (numbered x))
        where 
            oddx :: (Int,a) -> Bool 
            oddx (n,i)
             | odd n = False 
             |otherwise = True 


-- takes two lists and returns the positions of where elements coincide.
-- eg "Mary" "Jane" would return [2]

same :: Eq a => [a] -> [a] -> [Int]
same [] _ = [0]
same _ [] = [0]
same x y = map (\i -> i+1) (map fst (map fst (filter takeeq (zip (numbered x) (numbered y)))))
        where 
            takeeq :: Eq b => ((a,b),(a,b)) -> Bool 
            takeeq ((j,i),(p,q)) = i == q   

      