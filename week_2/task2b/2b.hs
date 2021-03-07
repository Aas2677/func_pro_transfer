

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Alex","Charles","Fitzwilliam","George","William"]
couples   = [("Elizabeth","Fitzwilliam"),("Charlotte","William"),("Lydia","George"),("Jane","Charles")]

------------------------- Exercise 1


ditch :: Int -> [a] -> [a]
ditch x [] = []
ditch 0 x = x  
ditch n (x:xs) = ditch (n-1) xs

at :: [a] -> Int -> a
at [] _ = error "error"
at (x:xs) 0 = x
at (x:xs) n = at xs (n-1)


------------------------- Exercise 2

find :: Eq a => a -> [(a,b)] -> b
find _ [] =  error "not in here"
find  x ((a,b):xs)
   | a == x    = b
   | otherwise = find x xs





which :: Eq a => a -> [a] -> Int
which = aux 0
  where
    aux :: Eq a => Int -> a -> [a] -> Int
    aux p q [] = error "not in here"
    aux p q (x:xs)
       | q == x   = p
       | otherwise  = aux (p+1) (q) (xs)



-- 2 c 

member :: Eq a => [a] -> a -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y 


remove :: Eq a => [a] -> a -> [a]
remove [] x = []
remove (x:xs) y
  | x == y = xs 
  | otherwise = x: remove xs y






before :: Ord a => [a] -> [a] -> Bool
before _ [] = False
before [] _ = False
before (x:xs) (y:ys) 
   | x < y     = True
   | x == y    = before xs ys
   | otherwise =  False



sorted :: Ord a => [a] -> Bool
sorted [_] = True
sorted (x:xs) =  (before [x] [(head xs)] || (x == head xs)) && sorted xs 




--q3

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) ( y:ys)
 | x < y  = x: merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

 

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus xs (y:ys) = minus (remove xs y) (ys)







msort :: Ord a => [a] -> [a]
msort xs
 | length xs <=1 = xs
 | otherwise = merge ( msort (first xs)) (msort(second xs))
     where 
       first xs  = let i = length xs in take (i `div` 2) xs 
       second xs = let i = length xs in drop (i `div`2) xs




-- ?????
--  | otherwise = merge (msort ((splitAt(((length (x:xs)+1)`div` 2 )(x:xs)) `at` 0)))  (msort (((splitAt(((length (x:xs)+1)`div` 2 )(x:xs)) `at` 1) )))

