
import Lib
import Data.List
{-
main :: IO ()
main = do
  x <- readFile "input.txt"
  print . reduce . solve . solve' . parseInput $ x 

parseInput :: String -> [Int]
parseInput = map read . lines

data Change = Increase | Decrease 
  deriving (Eq, Show) 

solve :: [Int] -> [Change]
solve (x : y : z) = (if (y > x ) then Increase else Decrease) : solve (y : z)
solve _ = []

solve' :: [Int] -> [Int]
solve' (x : y : z : xs) = (x + y + z) : solve' (y : z : xs)  
solve' _ = []

reduce :: [Change] -> Int
reduce = length . filter (Increase ==)
-}






{- get data from each direction, up n, down n, forward n    could possibly add up/down on own then minus down from up? ()
So we want, Read file, Add all forward, take away all down from up 
-}
submarine :: IO ()
submarine = do 
  a <- readFile "submarine.txt"
  print . solve


parseInput :: String -> submarineDir Int
parseInput = map read . lines  --- I want the dir + the int

type Depth = Int     -- verti movement the 2 values I want added
type Movement = Int  -- hori movement

data SubmarineDir =  Forward Int | Up Int | Down Int    -- the directions 
  deriving (Eq, Show)

SubmarineDir = dist . words
  where 
   dist ["forward", x] = Forward (read x) 
   dist ["up", x] = Up (read x)
   dist ["down", x] = Down (read x)


direction :: [SubmarineDir] -> (Depth , Movement)
direction (Forward , x) _ = (0 + x)
direction (Up , x)      _ = (x + x)
direction (Down , x )    _ = (x - x)

 --solve2 :: [Int] -> SubmarineDir
 -- solve2  = Movement * Depth 



--if submarineDir = "forward"
--moveForward :: [Char] -> [Int]

--if/else for up/down if up add else minus





-- break down so add three at a  time at first? then go from them? 


listTimes2 = [x * 2 | x <- [1..20]]


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