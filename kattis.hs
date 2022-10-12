module F1 where
import Data.Char

-- 1 Fibonnaci talen.

fib :: Integer ->  Integer
fib 0 = 0
fib 1 = 1
fib n | even n         = f1 * (f1 + 2 * f2)
      | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
      | otherwise      = (2 * f1 + f2) * (2 * f1 - f2) - 2
   where k = n `div` 2
         f1 = fib k
         f2 = fib (k-1)
 
-- 2 Rövarspråket. 
 
ickeVokal :: Char -> Bool
ickeVokal x = x `notElem` "aeiouy"      
 
rovarsprak :: String -> String
rovarsprak [] = []                      
rovarsprak (x:xs)
    | ickeVokal  x   = x:'o': x: rovarsprak xs
    | otherwise      = x: rovarsprak xs 
    
karpsravor :: String -> String
karpsravor [] = []                     
karpsravor (x:xs)
    | ickeVokal x     = x: karpsravor(tail(tail xs))      
    | otherwise       = x: karpsravor xs
 
--3 Medellängd.
 
medellangd :: String -> Double
medellangd s =  fromIntegral (sum listan) / fromIntegral (length listan)
  where listan = map length (words (orddelare s))               
-- words breaks a string up into a list of words, which were delimited by white space.
 
orddelare :: String -> String
orddelare [] = []                         
orddelare (x:xs)
   | isAlpha x  = x: orddelare xs         -- is Alphabet? true insert x then keep going to the next.
   | otherwise  = ' ': orddelare xs       --              false insert ' ' then keep going.
 
-- 4 Listskyffling.

-- Function to get the Every Other number the given list for example all odd numbers at the beggining
everother :: Int -> [a] -> [a]
everother n [] = []
everother n s  = head s : everother n (drop n s)
 
skyffla :: [a] -> [a]
skyffla [] = []
skyffla s  = everother 2 s ++ skyffla (everother 2 (tail s))