import GHC.StgToCmm.Layout (slowCall)
import GHC (HsExpr(NegApp), GhcMode (OneShot), showRichTokenStream, ABExport, abandon)
simple x y = x + y + y 
-- \x -> x 



calcChange owed given = if change > 0
                        then change
                        else 0
  where change = given - owed

inc x = x + 1
double x = x + x
square x = x * x

ifeve n = if even n
                  then n - 2
                  else 3 * n + 1 

sumSquareOrSquareSum x y = if sumSquare > squareSum 
                           then sumSquare
                           else squareSum
  where sumSquare = x^2 + y^2
        squareSum = (x+y)^2

sumSquareOrSquareSum' x y = (\sumSquare squareSum ->
                              if sumSquare > squareSum
                              then sumSquare
                              else squareSum) (x^2 + y^2) ((x+y)^2)

-- doubleDouble x = dubs *2 
-- where dubs = x*2 

doubleDouble' x = (\dubs -> dubs*2) (x*2)

overwrite x = (\x -> 
                           (\x ->
                             (\x -> x) 4
                            ) 3
                          ) 2

overwrite' x = let x = 2
               in
                let x = 3 
                in 
                 let x = 4 
                 in 
                    x

counter x = let x = x + 1
            in 
                let x = x + 1
                in
                 x 
{--
counter' (\x ->  x + 1)
         ((\x -> x + 1)
          ((\x -> x) x))
--}
          

subtract2 = flip (-) 2

binaryPartialApplication binaryFunc arg = (\x -> binaryFunc arg x)
-- takeFromfour = binaryPartialApplication (-) 4 

wardsInfinity = reverse [1..]

isPalindrome word = word == reverse word

takeLast n aList = reverse (take n (reverse aList))

repeat n = cycle [n] 

subseq start end myList = take difference (drop start myList) 
  where  difference = end - start 

{--
inFirstHalf val myList = val 'elem' firstHalf 
  where midpoint = (length myList) '/' 2
         firstHalf = take midpoint myList
         

myGCD a b = if remainder == 0
                           then b 
                           else myGCD b remainder 
    where remainder = a 'mod' b 
--}

robot (name, attack, hp) = \message -> message (name, attack, hp)
killerRobot = robot ("Kill3r", 25, 200)
gentleGiant = robot ("Mr. Friendly", 10, 300)
fastRobot = robot ("Speedy", 15, 40)
slowRobot = robot ("Slowpoke", 20, 30)

name (n, _, _) = n 
attack (_, a, _) = a
hp (_, _, hp) = hp 

getName aRobot = aRobot name 
getAttack aRobot = aRobot attack 
getHP  aRobot = aRobot hp 

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHp aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

printRobot aRobot = aRobot (\(n, a, h) -> n ++ 
                                                                                  " attack: " ++ (show a) ++
                                                                                  "hp: " ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) ->
                                                                                      robot (n, a, h-attackDamage))

fight aRobot defender = damage defender attack 
  where attack = if getHP aRobot > 10 
                                 then getAttack aRobot
                                 else 0 

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

fastRobotRound1 = fight slowRobot fastRobot
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2


--makeAddress :: Int  -> [Char]-> [Char] -> (Int, [Char], [Char])
makeAddress :: a -> b -> c -> (a, b, c)
makeAddress number street town = (number, street, town) 

makeAddressLambda = (\number ->
                                                   \ street ->
                                                     \town -> (number, street, town)) 


{-- patientInfo :: String -> String -> Int -> Int -> String 
patientInfo fname lname age height = name ++ " " ++ ageHeight
    where name = lname ++ ", " ++ fname 
                   ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in. )" 
                   
patientInfo :: FirstName -> LastName -> Age  -> Height -> [Char]
patientInfo fname lname age height = name ++ " " ++ ageHeight
    where name = lname ++ ", " ++ fname 
                   ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in. )" 
                   --}
  {-                 
patientInfoNAH :: PatientName -> Age -> Height -> [Char]
patientInfoNAH  (fname, lname) age height = name ++ " " ++ ageHeight 
    where name = lname ++ ", " ++ fname 
                  ageHeight = " (" ++ show age ++ "yrs. " ++ show height ++ "in. )"
                  
type FirstName = [Char] 
type LastName = [Char]
type Age = Int 
type Height = Int 
type PatientName = ([Char], [Char])

firstName :: PatientName -> [Char] 
firstName patient = fst patient
lastName :: PatientName -> [Char]
lastName patient = snd patient
--}

data Sex = Male | Female 
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType
sexInitial :: Sex -> Char 
sexInitial Male = 'M'
sexInitial Female = 'F'

patient1BT :: BloodType
patient1BT = BloodType  A Pos

patient2BT :: BloodType
patient2BT = BloodType O  Neg 

patient3BT :: BloodType
patient3BT = BloodType AB Pos  

showRh :: RhType -> [Char] 
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> [Char]
showABO A =  "A" 
showABO B = "B"
showABO AB = "AB"
showABO O = "O" 
showBlootType :: BloodType -> [Char] 
showBlootType  (BloodType abo rh) = showABO abo ++ showRh rh 


