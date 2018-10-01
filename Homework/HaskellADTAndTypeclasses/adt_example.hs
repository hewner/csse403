
-- ADT Example - file structures
-- ADTs are discussed in your textbook but there also is a good discussion here
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types

-- Make a FileObject ADT



-- Your definition should make the examples below work

exampleFile = File "someData.dat"
exampleSymlink = Symlink "/etc/fooConfig"
littleDirectory = Directory "littleDir" [File "a.txt", File "b.txt"]
bigDirectory = Directory "bigDir" [File "foo.txt", Directory "otherDir" [ File "littleFile.txt", littleDirectory], Symlink "../foo.txt"] 

-- write a function that counts the number of files in a FileObject
-- recursing throgh directories but ignoring symlinks.  Only files
-- count, not directories or symlinks.

countFiles :: FileObject -> Int
countFiles _ = 0

-- test code below

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual
  | expected == actual = putStrLn (testName ++ " PASSED")
  | otherwise =
    do
      putStrLn $ testName ++ " FAILED"
      putStrLn $ "Expected: " ++ (show expected)
      putStrLn $ "Actual: " ++ (show actual)

main = assertEqual "only1Test" 4 (countFiles bigDirectory)
