{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- | Applies a function to a monadic value. 
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do
            x <- mx
            return $ f x

-- | Safe swap of elements at indicies i and j of a vector.
--   If either index is out of bounds, return Nothing. 
swapV :: (Eq a) => Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j vec 
    | (vec !? i) == Nothing = Nothing
    | (vec !? j) == Nothing = Nothing
    | otherwise = Just (vec // swap)
                  where swap = [(i, (!) vec j),(j, (!) vec i)]     


-- | Applies a monadic function to a list, then sequences 
--   the results. 
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
              x'  <- f x
              xs' <- mapM f xs
              return (x':xs')

-- | Given a list of indices, return a list of the elements
--   in the vector at those indicies. If there are any indices
--   which are out of bounds, then return Nothing. 
getElts :: (Eq a) => [Int] -> Vector a -> Maybe [a]
getElts indices vec 
    | flst      = Nothing
    | otherwise = let lst' = zipWith (!) vec' indices
                   in mapM Just lst'
    where vec' = replicate (length indices) vec
          lst  = zipWith (!?) vec' indices
          flst = foldr flst' False lst
          flst' = (\x acc -> if x == Nothing then True else acc)


-- | Boilerplate for random monad. 
type Rnd a = Rand StdGen a

-- | Returns a random element from a given Vector.
--   If the vector is empty, return Nothing.
randomElt :: Vector a -> Rnd (Maybe a)
randomElt vec 
    | (V.length vec) == 0 = do
                              x <- getRandom
                              let x' = vec !? x 
                                in return x'
    | otherwise           = do
                              x <- getRandomR (start, end)
                              let x' = vec !? x 
                                in return x'
     where start = 0
           end   = (V.length vec) - 1   

-- | Given an int n, output a random vector of length n. 
randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n 
    | n < 0 = error "No negative lengths."
randomVec 0 = return $ V.fromList []
randomVec n = do
            x  <- getRandom
            xs <- randomVec (n - 1)
            return (cons x xs)

-- | Given an int n, and a range of possible values, output
--   a random of vector of length n, whose entries are in 
--   the specified range.
randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n _ 
    | n < 0 = error "No negative lengths."
randomVecR 0 _     = return $ V.fromList []
randomVecR n range = do
                   x  <- getRandomR range
                   xs <- randomVecR (n - 1) range
                   return (cons x xs)


-- | Given a vector, output a vector with the elements permuted.
--   Uses Fisher-Yates algorithm to ensure shuffling is uniform,
--   that is, each element has an equal probability of being in 
--   any position in the new vector. 
shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = foldr randomSwap vec' [1..(V.length vec - 1)]
            where vec' = return vec
                  randomSwap i acc = do 
                                   j <- getRandomR(0,i)
                                   acc' <- acc  
                                   return $ unsafeSwapV i j acc'

-- | Helper function for shuffle, same as SwapV, only unsafe.
--   Assumes indicies are valid. 
unsafeSwapV :: Int -> Int -> Vector a -> Vector a
unsafeSwapV i j vec = vec // swap
                where swap = [(i, (!) vec j),(j, (!) vec i)] 


-- | Given a vector, and the index of a "pivot," output a triple.
--   The first element is a vector of elements less than the pivot.
--   The second element is the pivot element in the vector.
--   The third element is a vector of elements greater than or equal
--   to the pivot. 
partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec i = 
  (V.filter lt vec' , vec ! i , V.filter geq vec')
  where vec' = (V.take i vec) V.++ (V.drop (i + 1) vec)
        lt x  = if x < (vec ! i) then True else False
        geq x = if x >= (vec ! i) then True else False
 

-- | Quicksort on vectors.
qsort :: Ord a => Vector a -> Vector a
qsort vec 
    | V.length vec == 0 = V.empty
qsort vec = qsort lt V.++ (V.singleton x) V.++ qsort geq
            where x   = V.unsafeHead vec
                  xs  = V.tail vec
                  lt  = V.filter (< x) xs
                  geq = V.filter (>= x) xs


-- | Randomized quicksort on vectors.
qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec | V.length vec == 0 = do return V.empty
qsortR vec = do 
               x <- getRandomR(0, V.length vec - 1)
               let vec'  = partitionAt vec x 
                   pivot = V.singleton (second vec')
               lt  <- qsortR $ first vec'
               geq <- qsortR $ third vec'
               return (lt V.++ pivot V.++ geq)
             

-- | Gets first element of a triple.
first :: (a,b,c) -> a
first (x,_,_) = x

-- | Gets second element of a triple.
second :: (a,b,c) -> b
second (_,x,_) = x

-- | Gets third element of a triple.
third :: (a,b,c) -> c
third (_,_,x) = x              


-- | Randomized select on vectors.
--   Gets element of rank i from vector, where
--   0 is the min, (length vector - 1) is the max.
--   If index is out of bounds, then output Nothing.
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i vec 
    | i < 0             = return Nothing
    | i >= V.length vec = return Nothing
select i vec = do 
             x <- getRandomR(0, V.length vec - 1)
             let vec' = partitionAt vec x 
                 lt   = first vec'
                 geq  = third vec'
                 m    = V.length lt
              in if i == m 
                  then return $ Just (second vec')
                  else if i < m
                    then select i lt
                    else select (i - m - 1) geq


-- | Returns a deck of cards in order from least to
--   greatest by the value of the card (2..Ace), and
--   by suit (Spade, Heart, Club, Diamond).
allCards :: Deck
allCards = [(Card x y)| x <- labels, y <- suits]

-- | Shuffles a new deck of cards. 
newDeck :: Rnd Deck
newDeck = shuffle allCards


-- | Given a deck, returns a tuple of the card drawn,
--   and the rest of the deck.
--   If the deck was empty, returns Nothing.
nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck 
    | V.null deck = Nothing
    | otherwise   = Just (V.head deck, V.tail deck)


-- | Draws n cards from the deck, if possible.
getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck 
  | n <= 0 = Nothing
  | V.null deck = Nothing
  | otherwise   = exchange n (nextCard deck) (nextCard' deck)

-- | Helper function for getCards, gets an empty hand
nextCard' :: Deck -> Maybe ([Card], Deck)
nextCard' deck 
  | V.null deck = Nothing
  | otherwise   = Just ([], deck)

-- | Takes an integer n, a tuple containing the top card and the rest of the deck,
--   and a new hand, and keeps adding the top card to the new hand until n equals zero.
exchange :: Int -> Maybe (Card, Deck) -> Maybe ([Card], Deck) -> Maybe ([Card],Deck)
exchange _ Nothing _ = Nothing
exchange _ _ Nothing = Nothing
exchange 0 _ old = old 
exchange n new old  = do
                   new' <- new
                   old' <- old 
                   let deck     = snd new' 
                   let newCard  = fst new'
                   let oldCards = fst old'
                   let arg1     = nextCard deck
                   let arg2     = return ((newCard : oldCards), deck)
                   result <- exchange (n-1) arg1 arg2
                   return result
