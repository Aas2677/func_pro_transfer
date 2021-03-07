data List a = Nil | Cons a (List a)


maplist :: (a->b) -> List a -> List b 
maplist f Nil = Nil  
maplist f (Cons x xs) = Cons(f x) (maplist f xs)



-- Binary tree types 

data IntTree = Empty | Node Int IntTree IntTree
     deriving Show 


size :: IntTree -> Int 
size Empty = 0
size (Node x left right) = size left + size right + 1

total :: IntTree -> Int 
total Empty = 0 
total (Node x left right) = total left + total right + x


treemap :: (Int -> Int) -> IntTree -> IntTree
treemap f Empty = Empty
treemap f (Node x left right) = 
    Node (f x) (treemap f left) (treemap f right)



-- ordered trees

--inserting into an ordered tree 
insertT :: Int -> IntTree -> IntTree
insertT x Empty = Node x Empty Empty
insertT x (Node y left right )
        | x <=y = Node y (insertT x left) right 
        |otherwise = Node y left (insertT x right)

--build tree from a list
build :: [Int] -> IntTree
build [] = Empty
build (x:xs) = insertT x (build xs)

smallest :: IntTree -> Int 
smallest Empty = error "Can't find the min of an empty tree"
smallest (Node x Empty right) = x 
smallest (Node x left right) = smallest left 


-- isordered :: IntTree -> Bool 
-- isordered EMpty = True 
-- isordered 


-- extracting numbers from the tree 

flatten :: IntTree -> [Int]
flatten Empty = [] 
flatten (Node x left right) = flatten left ++ [x] ++ flatten right   


treeSort :: [Int] -> [Int]
treeSort = flatten . build


-- arithmetic expression trees
--datatype

data Expr = Num Int 
         | Plus Expr Expr
         | Minus Expr Expr
         | Times Expr Expr
 deriving Show


-- evaluating arithmetic expressions 
eval :: Expr -> Int 
eval (Num x)   =  x  
eval (Plus e1 e2) =  eval e1 + eval e2 
eval (Minus e1 e2) = eval e1 - eval e2 
eval (Times e1 e2) = eval e1 * eval e2  


-- implement the distributive laws

distribute :: Expr -> Expr
distribute (Times e1 (Plus e2 e3)) = Plus (Times e1 e2) (Times e1 e3)
distribute e = e 

