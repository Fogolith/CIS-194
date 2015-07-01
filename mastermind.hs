{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

{- A peg can be one of six colors. -}
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)


{- A code is defined to simply be a list of Pegs. -}
type Code = [Peg]


{- A move is constructed using a Code and two integers; the number of
   exact matches and the number of regular matches. -}
data Move = Move Code Int Int
          deriving (Show, Eq)


{- List containing all of the different Pegs. -}
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]


{- Get the number of exact matches between the actual code and the guess. -}
exactMatches :: Code -> Code -> Int
exactMatches code1 code2 = sum $ zipWith (\x y -> if x == y then 1 else 0) code1 code2 


{- For each peg in xs, count how many times it occurs in ys. -}
countColors :: Code -> [Int]
countColors code = foldr counter [] colors
                   where counter x acc = (length $ filter (== x) code) : acc 


{- Count number of matches between the actual code and the guess. -}
matches :: Code -> Code -> Int
matches code1 code2 = sum $ zipWith (min) (countColors code1) (countColors code2)


{- Construct a Move from a guess given the actual code. -}
getMove :: Code -> Code -> Move
getMove code1 code2 = Move code2 (exactMatches') (matches' - exactMatches')
                      where matches'      = matches code1 code2
                            exactMatches' = exactMatches code1 code2


{- Checks if a code is consistent with a given move. -}
isConsistent :: Move -> Code -> Bool
isConsistent move@(Move code1 _ _) code2 = (getMove code2 code1) == move


{- Filters the list of all possible codes by consistency with a given move. -}
filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes


{- Given an integer n, generates a list of all possible codes of length n. -}
allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes n = foldr newCodes	[] colors
             where newCodes x acc = map (x:) (allCodes (n-1)) ++ acc


{- Given a code of length n, return a list of Moves consistent to that code. -}
solve :: Code -> [Move]
solve code = filter (isConsistent' code) $ map (getMove code) $ allCodes $ length code

{- isConsistent with order of inputs reversed, to take advantage of partial 
   application in the solve function. -}
isConsistent' :: Code -> Move -> Bool
isConsistent' code2 move@(Move code1 _ _) = (getMove code2 code1) == move
