--Challenges
--1)  1234 -> [4,3,2,1]
reverseNumbers :: Integer -> [Integer]
reverseNumbers num
            | num == 0 = []
            | otherwise = num `mod` 10 : reverseNumbers (num `div` 10)

reverseNumbers' :: [Integer] -> Integer -> [Integer]
reverseNumbers' accum num
            | num == 0 = accum
            | otherwise = reverseNumbers' (accum ++ [(num `mod` 10)]) (num `div` 10)

--2) "ABBA" -> True; "Queen" -> False
palindrome :: [Char] -> Bool
palindrome arr
         | length arr <= 1 = True
         | head arr /= last arr = False
         | otherwise = palindrome (init (tail arr))

--3) find max [1,10,9,7,15,3] -> 15
max' :: [Integer] -> Integer
max' arr
   | length arr <= 1 = head arr
   | otherwise = max' (dropMinHeadLast arr)

dropMinHeadLast :: [Integer] -> [Integer]
dropMinHeadLast arr
              | head arr > last arr = init arr
              | otherwise = tail arr  
-- 4) power of two  2->True; 14 -> False; 16 -> True
powOfTwo :: Integer -> Bool
powOfTwo num
       | num < 1 = False
       | num == 2 = True
       | mod num 2 == 0 = powOfTwo (div num 2)
       |otherwise = False
