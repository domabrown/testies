
import Lib
import Data.List

main :: IO ()
main = do
  x <- readFile "input.txt"
  print . reduce . solve . parseInput $ x 

parseInput :: String -> [Int]
parseInput = map read . lines

data Change = Increase | Decrease 
  deriving (Eq, Show) 

solve :: [Int] -> [Change]
solve [] = []
solve [x] = []
solve (x : y : zs) = (if (y > x ) then Increase else Decrease) : solve (y : zs)  

reduce :: [Change] -> Int
reduce x = length $ filter (== Increase) x


doubleMe x = x + x
doubleUs x y = x*2 + y*2
doublesmallNumber x = if x > 100
                        then x
                        else x*2
removeNonUppercase dj = [ c | c <- dj, c `elem` ['A' .. 'Z']]

-- first' :: [a] -> a 
-- first' [] = error "No first fuck face"
-- first' (x:_) = x 

length' :: (Num b) => [a] -> b
length' [] = 0 
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a 
sum' [] = 0 
sum' (t:ts) = t + sum' ts

-- Get this one to give different answer if the list is empty
sums' :: (Num a) => [a] -> a
sums' = foldr(+) 0 


--numGuess :: (RealFloat a) => a -> String 
--numGuess numb 
  -- | numb <= 17.5 = "cold motherfucker" 
  -- | numb <= 20.1 = "warmish"
  -- | numb <= 22.5 = "dingaling"
  -- | numb <= 24.0 = "warm bruh"
  -- | numb <= 26.0 = "no way m8"
  -- | otherwise = "don't even bother trying"

larger' :: (Ord a) => a -> a -> a
larger' a b 
    | a > b    = a 
    |otherwise = b

bmiTell :: (RealFloat a) => a -> a -> String 
bmiTell weight height 
    | bmi <= 18.5 = "Under"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "over"
    | otherwise   = "you absolute fat cunt"
    where bmi = weight / height ^ 2