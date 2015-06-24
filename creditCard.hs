{- A credit card number validation algorithm. -}

{- Turns the credit card number into a list. It is in reversed order
so we can take advantage of the efficiency of the ":" operator. -}
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
	| x <= 0    = []
	| otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

{- Puts the list of credit card digits in the right order. -}
toDigits :: Integer -> [Integer]
toDigits x = reverse $ toDigitsRev x

{- Helper function for doubleEveryOther, which has a 1 if the 
associated number in the credit card digit list will stay the same,
and a 2 if it will be doubled. -}
mapping :: Int -> [Integer]
mapping n 
	| n <= 0    = []
	| even n    = 2 : mapping (n-1)
	| otherwise = 1 : mapping (n-1)

{- Doubles every other number in the list, starting with the right.
Example: [1,2,3] -> [1,4,3], [1,3,5,7] -> [2,3,10,7] -}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = zipWith (*) lst (mapping (length lst))

{- Sums all of the digits in the list. 
Example: [1,2,3,4] -> 10, [1,11,12,2] = 8 -}
sumDigits :: [Integer] -> Integer
sumDigits lst = sum $ concat $ map toDigits lst

{- The actual validation algorithm, derived from all of the other functions.
Checks if the resulting number from all of the transformations is divisible by 10.
If yes, it is a valid credit card number, otherwise, it is not. -}
validate :: Integer -> Bool
validate n = 0 == (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10
