{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List
newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

-- x is a polynomial of degree 1, so it should be represented as 1x + 0
x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

-- Checking for polynomial equality
-- Trivial if they do not have the same number of terms
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P f) (P g) 
        | length f /= length g = False
        | otherwise            = f == g
 
-- Exercise 3 -----------------------------------------

-- Allows polynomial types to be read as we normally read them in the REPL
-- Example: P [1,2,3] = 3x^2 + 2x + 1
-- Example: P [1,0,4] = 4x^2 + 1
-- Example: P [1,1,1] = x^2 + x + 1
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P [a])   = show a
    show (P f)     
        | lead == 1   = "x^" ++ power ++ " + " ++ recurse
        | lead == 0   = recurse 
        | otherwise   = (show $ lead) ++ "x^" ++ power ++ " + " ++ recurse
        where power   = show (length f - 1) --power of leading term
              recurse = show (P (init f))   --recursive step in function
              lead    = last f              --coefficient of leading term

-- Exercise 4 -----------------------------------------

-- Allows polynomials to be added in list form
-- if one polynomial has less terms, we simply add zeroes to the end until
    -- the lengths are equal
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P f) (P g) 
    | fLen == gLen  = P (adder f g)
    | fLen < gLen   = P (adder fWithZero g)
    | otherwise     = P (adder f gWithZero)
    where adder a b = zipWith (+) a b
          fWithZero = plusHelper (gLen - fLen) f
          gWithZero = plusHelper (fLen - gLen) g
          fLen      = length f
          gLen      = length g

plusHelper :: Num a => Int -> [a] -> [a]
plusHelper 0 poly = poly
plusHelper n poly = (plusHelper (n - 1) poly) ++ [0] 
 
-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = 0
times (P (a:as)) (P f) = (P (map (*a) f)) + times (P as) (P (0:f))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P f)  = (P (map negate f))
    fromInteger n = (P [fromIntegral n])
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P [])  _ = error "Not a polynomial."
applyP (P [n]) _ = n
applyP (P f) n   = (last f) * (n ^ (power f)) + applyP (P g) n  
                 where power h = length h - 1
                       g       = init f

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f
        | n < 0     = error "No such thing as a negative derivative."
        | n == 0    = f
        | n == 1    = deriv f
        | otherwise = deriv $ nderiv (n-1) f

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P [])  = error "Not a valid polynomial."
    deriv (P f)   = (P (reverse $ derivHelper f))

derivHelper :: Num a => [a] -> [a]
derivHelper [y] = [0]
derivHelper f   = (genericLength f - 1) * (last f) : derivHelper (init f)
