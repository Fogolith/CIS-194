-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches code1 code2 = sum $ zipWith (\x y -> if x == y then 1 else 0) code1 code2 

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
--countColors code = [length $ filter (== Red) code, length $ filter (== Green) code, length $ filter (== Blue) code, length $ filter (== Yellow) code, length $ filter (== Orange) code, length $ filter (== Purple) code]
countColors code = foldr counter [] colors
                   where counter x acc = (length $ filter (== x) code) : acc 

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code1 code2 = sum $ zipWith (min) (countColors code1) (countColors code2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove code1 code2 = Move code2 (exactMatches code1 code2) ((matches code1 code2) - (exactMatches code1 code2))

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move code1 _ _) code2 = (getMove code2 code1) == move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined
