  
  sqDist :: (a,b) -> Maybe Int   -- takes 2 ints and gives an int? why not Int->Int->Int->
  sqDist (x,y) = x^2 + y^2 

  sq :: Int -> Int 
  sq x = x*x 
  sqPrint = print $ sq $ 2 + 3
  main = print (sqDist (3,4))
  dist = sqrt . sqDist
  flop (x, y) = (y, x)