

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]
gents2  =  ["Charles","Fitzwilliam","ziggy","George","William"]
test1 = ["George","William"]
test2 = ["George","William","Alex"]

------------------------- Exercise 1

member :: [String] -> String -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y 




member' :: [String] -> String -> Bool
member'    []  _ = False   
member' (x:xs) y = (||) (y==x) (member xs y)

remove :: [String] -> String -> [String]
remove [] x = []
remove (x:xs) y
  | x == y = xs 
  | otherwise = x: remove xs y 


------------------------- Exercise 2

members :: [String] -> [String] -> Bool
members xs    []  = True
members xs (y:ys)
  | member xs y = members xs ys
  | otherwise = False




members' :: [String] -> [String] -> Bool
members' xs (y:ys) = (&&) (member xs y) (members xs ys)

removeAll :: [String] -> [String] -> [String]
removeAll xs [] = xs
removeAll xs (y:ys) = removeAll (remove xs y) (ys)


------------------------- Exercise 3

before :: [Char] -> [Char] -> Bool
before _ [] = False
before [] _ = False
before (x:xs) (y:ys) 
   | x < y     = True
   | x == y    = before xs ys
   | otherwise =  False




before' :: [Char] -> [Char] -> Bool
before' _ [] = False
before' [] _ = True
before'(x:xs) (y:ys) = (x < y) || (x == y ) && (before xs ys)


  

sorted :: [String] -> Bool
sorted [_] = True
sorted (x:xs) =  (before x (head xs) || (x == head xs)) && sorted xs 


