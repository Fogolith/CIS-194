{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List
newtype Poly a = P [a]


{- A polynomial typeclass, represented as lists.
   The first element is the constant term, the 
   second element is the linear term, and so on. -}
   

{- x is a polynomial of degree 1, so it should be represented as 1x + 0 -}
x :: Num a => Poly a
x = P [0,1]


{- Allows checking for polynomial equality.
   Trivial if they do not have the same number of terms. -}
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P f) (P g) 
        | length f /= length g = False
        | otherwise            = f == g
 

{- Allows polynomial types to be read and written 
   as we normally read/write them in the REPL.
   Example: P [1,2,3] = 3x^2 + 2*x + 1
   Example: P [1,0,4] = 4x^2 + 1
   Example: P [1,1,1] = x^2 + x + 1 -}
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P f) = showReverse $ reverse f

{- Helper function which produces string of desired 
   polynomial from reversed list. -}
showReverse :: (Num a, Eq a, Show a) => [a] -> String    
showReverse [] = error "Not a valid polynomial."
showReverse f@(y:ys)  
    | allZero f          = "0"
    | y == 0             = recurse
    | ys == [] && y /= 0 = show y
    | y == 1 && small    = "x" ++ " + " ++ recurse
    | small              = show y ++ "x + " ++ recurse
    | y == 1             = "x^" ++ power ++ " + " ++ recurse
    | otherwise          = show y ++ "x^" ++ power ++ " + " ++ recurse
         where power     = show (length f - 1) 
               recurse   = showReverse ys
               small     = length ys == 1 

{- Checks if the list is all zeroes. -}
allZero :: (Num a, Eq a, Show a) => [a] -> Bool
allZero f = foldl' (\acc y -> if y /= 0 then False else acc) True f


{- Allows polynomials to be added.
   If one polynomial, "f", has less terms than the 
   other, we simply add terms with zeroes as 
   coefficients to "f" until the lengths are equal. -}
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P f) (P g) 
    | length f == length g = P (addPoly f g)
    | length f < length g  = P (addPoly fWithZero g)
    | otherwise            = P (addPoly f gWithZero)
    where addPoly a b = zipWith (+) a b 
          fWithZero = addZero (length g - length f) f
          gWithZero = addZero (length f - length g) g

{- Helper function to add zero-coefficent terms to 
   the polynomial with less terms. -}
addZero :: Num a => Int -> [a] -> [a]
addZero 0 poly = poly
addZero n poly = (addZero (n - 1) poly) ++ [0]  

 
{- Allows for polynomials to be multiplied.
   If you multiply two polynomials, f and g, 
   you take each term in f and multiply it by
   each term in g.
   Example: 
   (x + 2) * (x + 4) = x*x + x*4 + 2*x + 2*4
                     = x^2 + 6*x + 8
   P [2,1] * P [4,1] = [2*4,2*1] + [1*0.1*4.1*1]
                     = [8,6,1] = x^2 + 6*x + 8 -}
times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = 0
times (P (y:ys)) (P f) = (P (map (*y) f)) + times (P ys) (P (0:f))


{- The 6 operations any "number" should have, 
   as applied to polynomials. We are only 
   defining the first four, as the absolute 
   value of a polynomial is no longer a 
   polynomial, and polynomials do not have a 
   sign. -}
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P f)  = (P (map negate f))
    fromInteger n = (P [fromIntegral n])
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined


{- Takes a polynomial and a value, and evaluates 
   the polynomial at that value. -}
applyP :: Num a => Poly a -> a -> a
applyP (P [])  _ = error "Not a polynomial."
applyP (P [n]) _ = n
applyP (P f) n   = (last f) * (n ^ (power f)) + applyP (P g) n  
                 where power h = length h - 1
                       g       = init f


{- Defines the Diffrentiable type class, and defines the 
   nderiv function, which takes the nth deriviative of a 
   polynomial. -}
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f
        | n < 0     = error "No such thing as a negative derivative."
        | n == 0    = f
        | n == 1    = deriv f
        | otherwise = deriv $ nderiv (n-1) f


{- Takes the derivative of a polynomial using the power rule. -}
instance Num a => Differentiable (Poly a) where
    deriv (P [])  = error "Not a valid polynomial."
    deriv (P [_]) = (P [0])
    deriv (P f)   = (P (tail $ reverse $ derivHelper f))

{- Helper function which gives the derivative in reverse order. -}
derivHelper :: Num a => [a] -> [a]
derivHelper [_] = [0]
derivHelper f   = (genericLength f - 1) * (last f) : derivHelper (init f)
