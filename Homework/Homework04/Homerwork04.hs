import System.Win32 (COORD(x))
-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

selectFromTupleList :: [([Int], [Int])] -> String
selectFromTupleList [(_,[_,k]),_] =  show k
selectFromTupleList _ = "nothing to select from"

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
removeFirst3 :: [a] -> [a]   
removeFirst3 (_:_:_:rest) = rest
removeFirst3 list         = list

removeFirst3' :: [a] -> [a]
removeFirst3' list = case list of
    (_:_:_:rest) -> rest
    fullList     -> fullList

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
sum3 :: (Integer, Integer, Integer) -> Integer
sum3 (a, b, c) = a + b + c 
-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
checkEmptyList :: [a] -> Bool
checkEmptyList [] = True
checkEmptyList _ = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
tail' :: [a] -> [a]
tail' (x:rest) = rest
tail' []      = []

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)
addOneToEven :: Int -> Int
addOneToEven x = case even x of
    True -> x + 1
    False -> x

-- Extra homework
