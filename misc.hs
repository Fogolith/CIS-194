{- Takes a list, and returns a list of lists where the nth element is a list
   containing every nth element of the original list. 
   Example: skips "Hello" = ["Hello","el","l","l","o"]  
   i.e. The first element is a list containing every element, 
   The second element is a list containing every second element, and so on.-}
skips :: [a] -> [[a]]
skips []  = [[]]
skips lst = zipWith (nth) [1..(length lst)] (listception (length lst) lst)

{- Takes an integer n, and a list, and returns a list of n copies of the original list. -}
listception :: Int -> [a] -> [[a]]
listception n lst
    | n < 1  = [[]]
    | n == 1 = [lst]
    | n > 1  = [lst] ++ listception (n-1) lst 

{- Takes an integer n, and a list, and returns a list with every nth element. -}
nth :: Int -> [a] -> [a]
nth _ []  = []
nth n lst = take 1 (drop (n-1) lst) ++ (nth n (drop n lst))


{- A local maximum is an element of a list that is strictly greater than the element
   to it's left, and the element to it's right. (i.e., the first and last element 
   cannot be a local maximum, because they only have one element next to it. 
   This function takes a list, and returns a list of local maxima. -}
localMaxima :: [Integer] -> [Integer]
localMaxima []    = []
localMaxima [x]   = []
localMaxima [x,y] = []
localMaxima (x:y:z:xs)
    | y > x && y > z = y : localMaxima (z:xs)
    | y < z          = localMaxima (y:z:xs)
    | otherwise      = localMaxima xs


{- Takes a list of numbers from 0-9, outputs a histogram of asterixes, showing
   the frequency of each digit in the list. 
   To view properly, use the putStr function, and parenthesis around the
   histogram function, and its parameter.
   Example: putStr (histogram [3,1,1,5]) -}
histogram :: [Integer] -> String
histogram []  = "==========\n0123456789\n"
histogram lst = (toAsterix . count0_9) lst ++ histogram []

{- A function which handles the asterix part of the processing for the histogram. 
   Takes a frequency list as input. -}
toAsterix :: [Int] -> String
toAsterix [0,0,0,0,0,0,0,0,0,0] = ""
toAsterix occ = toAsterix (map f occ) ++ foldr asterix "" occ ++ "\n"
                where f x 
                         | x > 0     = x - 1
                         | otherwise = 0
                      asterix x acc = if x > 0 then "*" ++ acc else " " ++ acc

{- Given a list of numbers ranging from 0-9, outputs a list of frequencies for 
   each digit.
   Example: [3,1,1,5] ->       [0,2,0,1,0,1,0,0,0,0]
            [1,4,0,4,5,9,8] -> [1,1,0,0,2,1,0,0,1,1]-}
count0_9 :: [Integer] -> [Int]
count0_9 lst = foldr counter [] digits
               where digits        = [0..9]
                     counter x acc =  (length $ filter (== x) lst) : acc 
