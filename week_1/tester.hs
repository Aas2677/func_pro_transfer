countdown :: Int -> [Int]
countdown x
 | x <= 0 = []
 |otherwise = x: countdown(x-2)

badlength :: [Int] -> Int 
badlength [] = 0
badlength(x:xs) = 1 + badlength(xs)



--pattern matching --


total :: [Int] -> Int 
total [] = 0
total (x:xs) = x + total xs 



grab :: Int -> [Int] -> [Int]
grab _ []    = []
grab n (x:xs)
 | n <= 0     = []
 | otherwise = x : grab (n-1) xs   




invert :: [(Int,String)] -> [(String,Int)]
invert [] = [] 
invert ((x,y):xs) = (y,x) : invert xs




-- mm curries --

-- curry--
pairToAlone :: ((Int,Int) -> Int) -> Int -> Int -> Int
pairToAlone f x y = f (x,y)

--uncurry--
aloneToPair :: (Int -> Int -> Int) -> (Int,Int) -> Int 
aloneToPair f (x,y) = f x y 


addPair :: (Int,Int) -> Int
addPair(x,y) = x + y 


addAlone :: Int -> Int -> Int
addAlone x y = x + y 




