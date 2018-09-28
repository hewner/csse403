-- Some of Haskell's main strenghs are typing and high order functions
-- (i.e. functions that operate on functions), so let's start there.
--
-- Implement these 4 functions.  ALSO be sure to specify the type
-- signature (that is, don't leave it type inference) for the ones I
-- left it off
--
-- You also might find this a handy reference http://learnyouahaskell.com/higher-order-functions


-- takes a boolean function and a list, return true if the function
-- holds true for any of the items in the list
-- BTW there is already a haskell "any" function that does this, but don't use it
myAny :: (a -> Bool) -> [a] -> Bool
myAny func (h:t) = False

-- takes a list of items and prints each of them on their own line
-- eg printOnTheirOwnLines [1,2,3] prints
--1
--2
--3
-- I'll give you some hints on this one
-- 1.  It's easier to do using recusion and do
-- 2.  My base case looks like this:
--       printOnTheirOwnLines [] = return ()
-- 3.  My type signature starts like this: printOnTheirOwnLines :: (Show a) => NORMAL TYPE SIGNATURE STUFF
printOnTheirOwnLines _ = return ()

  

-- takes a function that takes and returns the same type, a initial input, and a number of times
-- applies the function to itself that number of times and returns the final result
-- e.g. funPower someFun "foo" 3 is equivalent to (someFun (someFun (someFun "foo")))
-- you can assume times will never be < 0
funPower func initial times = initial

-- A function that takes a binary integer function and returns a new function
-- same as the old, except that it returns the absolute value of the result
-- note that when testing you have to put parens around negative numbers e.g. (-3)
absFunc func = func

-- I implmented this one for my testing
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual
  | expected == actual = putStr "."
  | otherwise =
    do
      putStrLn $ testName ++ " FAILED"
      putStrLn $ "Expected: " ++ (show expected)
      putStrLn $ "Actual: " ++ (show actual)
      
main :: IO ()
main = do
  assertEqual "any1" True (myAny (>3) [1,2,3,4,1])
  assertEqual "any2" False (myAny (>3) [1,2,3,0,1])
  assertEqual "any3" True (myAny (\x -> length x > 1) ["a","b","ab"])
  assertEqual "any4" False (myAny (\x -> length x > 1) ["a","b"])  

  assertEqual "funpower1" 9 (funPower (*3) 1 2)
  assertEqual "funpower2" 27 (funPower (*3) 1 3)
  assertEqual "funpower3" "aa" (funPower (\x -> x ++ x) "a" 1)
  assertEqual "funpower4" "aaaa" (funPower (\x -> x ++ x) "a" 2)
  assertEqual "funpower5" "aaaaaaaa" (funPower (\x -> x ++ x) "a" 3)

  assertEqual "absFunc1" 5 ((absFunc (+)) (-3) (-2))
  assertEqual "absFunc2" 6 ((absFunc (-)) 1 7)

  -- you have to look at the output to see if these work
  printOnTheirOwnLines [1,2,3]
  printOnTheirOwnLines ["foo","bar","baz"]
