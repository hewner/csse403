
-- Typeclasses are a little bit like java interfaces

-- except that they are "open" - i.e. you can add existing types to
-- your new interfaces
--
-- Look here for more info http://learnyouahaskell.com/making-our-own-types-and-typeclasses#typeclasses-102

-- So as every programmer knows, some values are cooler than others
--
-- For example, the Int 3 has 6 'cool points'
-- The Int 15 has 2 cool points
-- all other Ints have zero cool points

-- Booleans can also be cool.  True is worth 2 cool points.  But
-- you're even cooler if you're not trying to be cool so False is
-- actually worth 4 cool points.

-- Lists of cool things have a coolness value equal to the sum of
-- their contents
-- hint here's how you want to do the restriction "instance (Cool a) => Cool [a] where"

-- Finally let's add our own ADT, CoolPoints that is just an int with
-- a raw cool points score.

-- Make your own typeclass Cool that requires implementing a function
-- rateCoolness which returns a number of cool points for each thing
-- in the type.


-- test code begins

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual
  | expected == actual = putStr "."
  | otherwise =
    do
      putStrLn $ testName ++ " FAILED"
      putStrLn $ "Expected: " ++ (show expected)
      putStrLn $ "Actual: " ++ (show actual)
      
main = do
  assertEqual "cp1" 6 (rateCoolness (3 :: Int))
  assertEqual "cp2" 2 (rateCoolness (15 :: Int))
  assertEqual "cp3" 0 (rateCoolness (7 :: Int))
  assertEqual "cp4" 4 (rateCoolness False)
  assertEqual "cp5" 2 (rateCoolness True)
  assertEqual "cp6" 75 (rateCoolness (CoolPoints 75))
  assertEqual "cp7" 6 (rateCoolness [1 :: Int,2,3,4,5])
  assertEqual "cp8" 6 (rateCoolness [CoolPoints 1, CoolPoints 2, CoolPoints 3])
