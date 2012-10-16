-- Exercises from Chapter 1

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n


-- Exercise 1.13

count :: Char -> String -> Int
count c []     = 0
count c (x:xs) | c == x    = 1 + (count c xs)
               | otherwise = count c xs

-- Exercise 1.14  TODO compare with their solution

blowup :: String -> String
blowup [] = []
blowup xs = blowupn 1 xs

blowupn :: Integer -> String -> String
blowupn n []     = []
blowupn n (x:xs) = (concatn n x) ++ (blowupn (n+1) xs)

concatn :: Integer -> Char -> String
concatn 0 c = []
concatn n c = [c] ++ (concatn (n-1) c)

-- Exercise 1.17  TODO both solutions feel icky

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys 

substring :: String -> String -> Bool
substring xs (y:ys) = substring xs ys || prefix xs (y:ys)
substring xs y      = prefix xs y

--substring xs [] = prefix xs []
--substring xs ys | prefix xs ys = True
--                | substring xs (tail ys) = True
--                | otherwise = False


-- Exercise 1.20

lengths :: [[a]] -> [Int]
-- lengths []     = []
-- lengths (x:xs) = (length x) : (lengths xs)
lengths xs     = map (length) xs

-- Exercise 1.21   TODO how can I write this function with map?
--                 (other than basically using lengths and adding them up)

sumLengths :: [[a]] -> Int
sumLengths [] = 0
sumLengths (x:xs) = (length x) + sumLengths xs


