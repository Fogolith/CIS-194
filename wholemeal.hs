import Data.List

{- For every even number in the list, subtract by 2, and multiply them. -}
fun1 :: [Integer] -> Integer
fun1 = (product . map (+ (-2)) . map noOdd)
       where noOdd x
               | even x = x
               | odd x  = 1

{- Takes an integer n, outputs the sum of the even terms in its collatz sequence. -}
fun2 :: Integer -> Integer
fun2 = (sum . filter even . takeWhile (>1) . iterate collatz)
       where collatz n
               | even n = n `div` 2
               | odd n  = 3*n + 1                   


data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
                deriving (Show,Eq)

f :: Eq a => a -> Tree a -> Tree a
f x Leaf                  = (Node 0 Leaf x Leaf)
f x (Node n Leaf y Leaf)  = (Node (n + 1) (f x Leaf) y Leaf)
f x (Node n Leaf y right) = (Node n (f x Leaf) y right) 
f x (Node n left y Leaf)  = (Node n left y (f x Leaf))
f x (Node p left@(Node m leftL _ rightL) y right@(Node n leftR _ rightR))
    | m < n                           = (Node p (f x left) y right)
    | m > n                           = (Node p left y (f x right))
    | m == n                          = (Node (p + 1) (f x left) y right) 

f x (Node n left y right) 

foldTree :: Eq a => [a] -> Tree a
foldTree = foldr f Leaf

{- Performs xor on a list of booleans. -}
xor :: [Bool] -> Bool
xor = foldr oddTrue False
      where oddTrue x acc 
              | x == True  = not acc
              | x == False = acc 

{- Implements the map function using foldr. -}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []


{- Implements the Sieve of Sundaram, taking an integer n, and outputing all primes below 2n +2. -}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ [1..n] \\ (sieveSundaram' n) 

{- Helper function, finds all non primes below n. -}
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = filter (<= n) $ map (\(a,b) -> a + b + 2*a*b) $ cartProd [1..n] [1..n]

{- Given by CIS-194. Function to find the cartesian product of two lists. -}
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
