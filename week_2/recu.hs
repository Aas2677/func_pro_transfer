-- Add an element to the end of a list
snoc :: Int -> [Int] -> [Int]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys



-- reverse a list 

myreverse :: [Int] -> [Int]
myreverse [] = []
myreverse (x:xs) = snoc x (myreverse xs)



--insertion sort 


-- insert an element into a sorted list 
-- insert :: Int -> [Int] -> [Int]
-- insert x []                 = [x]
-- insert x (y:ys)
--  | x <= y    = x:y:ys
--  | otherwise = y : insert x ys








-- --  insertion  sort algo 
-- isort :: [Int] -> [Int]
-- isort [] = []
-- isort(x:xs) = insert x (isort xs)


isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)
    where
        insert x [] = [x]
        insert x (y:ys)
         | x <= y = x:y:ys
         | otherwise = y : insert x ys


factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)


-- countup :: Int -> [Int]
-- countup 0 = []
-- countup n = snoc n (countup(n-1))





-- ACCUMULATORS AND TAIL RECURSION:

fact :: Int -> Int
fact n  = afact 1 n
    where
        afact :: Int -> Int -> Int
        afact a 0 = a 
        afact a n = afact (a*n) (n-1)



-- Tail recursion 
--   functions like afact are what we call tail recursive.
--   This means the recursive call is the last thing the function has to compute: it is not buried inside a larger calculaiton,
--   tail recursion is much more efficient than general recursion.


-- lets try the tail recursive version of reversing a list 

 

-- The first argument is reversed list so far and the second is the list remainding to be processed

goodreverse :: [Int] -> [Int]
goodreverse xs = areverse [] xs
        where 
            areverse :: [Int] -> [Int] -> [Int]
            areverse a [] = a
            areverse a (x:xs) = areverse (x:a) xs 




countup :: Int -> [Int]
countup n = countdownrev [] n
      where 
          countdownrev n 0 = n  
          countdownrev a n = countdownrev (n:a) (n-1)
