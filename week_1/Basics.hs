-- module Basics where

------------------------- Exercise 1

square :: Int -> Int
square x = x * x 





pythagoras :: Int -> Int -> Int -> Bool
pythagoras a b c = (square(a) + square(b)) == square(c)


------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = x 
    | otherwise = x * factorial(x-1)

euclid :: (Int, Int) -> Int
euclid (x, y)

    | x == 0 || y == 0 = error("no sir!S")
    | x == y = y
    | x <  y = euclid(x, y-x)
    | x >  y = euclid(y, x-y)

power :: Int -> Int -> Int
power x y 
  | y < 0 = error("no")
  |  x == 0 && y == 1 = 1 
  | y == 0 = 1
  | x == 0 = 0 
  | y == 1 = x 
  | otherwise  = abs x * (power ( abs x) ((abs y)-1))





------------------------- Exercise 3

range :: Int -> Int -> [Int]
range x y 
 | y < x  = []
 | y == x = [y]
 | otherwise = x : range (x+1) y 


times :: [Int] -> Int
times [] = 1
times (x:xs) = x * times xs


fact :: Int -> Int
fact x = times(range 1 x )