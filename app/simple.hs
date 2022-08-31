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
                (x -> x) 4
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

counter' (\x ->  x + 1)
         ((\x -> x + 1)
          ((\x -> x) x))