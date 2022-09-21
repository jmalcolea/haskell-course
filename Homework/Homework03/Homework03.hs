-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
checkLimitConsume :: Double -> Double -> Double -> String
checkLimitConsume hourlykW hours limitMax 
    | hours > 24 = "There are only 24 hours a day. Use checkLimitConsume <hourlykW> <hours> <LimitkW>"
    | consume > limitMax = "To much consume. Incredible bill"
    | otherwise = "Keep going"
  where
    consume = hourlykW * hours 

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.
checkExcessConsume :: Double -> Double -> Double -> String
checkExcessConsume hourlykW hours limitMax 
    | hours > 24 = "There are only 24 hours a day. Use checkLimitConsume <hourlykW> <hours> <LimitkW>"
    | consume > limitMax = "To much consume. You excess over limit on " ++ show excessconsume ++ "kW"
    | otherwise = "Keep going"
  where
    consume = hourlykW * hours  
    excessconsume = consume - limitMax 

-- In the previous function, return the excess/savings of consumption as part of the message.

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
buildQuery :: String -> String -> String -> String
buildQuery selectClause fromClause whereClause =
  let 
    ckeckSentence  
      | selectClause == "" || fromClause == "" = False
      -- other sintactic validations
      | otherwise = True
    composeQuery = "SELECT " ++ selectClause ++ " FROM " ++ fromClause ++ " WHERE " ++ whereClause
  in 
    if ckeckSentence then composeQuery else "Not posible to build query"
  
  
-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  
quotient :: Double -> Double -> String
quotient x y 
    | x == 0 && y == 0 = "Indetermined"
    | x == y = "same values not allowed because result would not be under 1"
    | otherwise =
      let 
          denominator x y = if x >= y  then x else y
          numerator x y = if x >= y  then y else x
      in  
          if x /= 0 && y /= 0 then show (numerator x y / denominator x y) else "0"  


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 
