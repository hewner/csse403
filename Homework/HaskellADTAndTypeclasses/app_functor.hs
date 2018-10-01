
import Control.Applicative

-- So in this example we have the idea of "safe" and "unsafe" values

data MaybeSafe a = Safe a | Unsafe a deriving (Show, Eq)

-- The kind of value where this arises when you've got user supplied
-- data that could have malicious code (e.g. SQL or javascript) and
-- needs to be output in some special way.  Then you've got other
-- values - say programmer written javascript or SQL that is known to
-- be safe.  But, if concat (say) user supplied values and programmer
-- values, the result should be considered unsafe.
--
-- In brief - a Safe value combined with a Safe value should result in
-- a Safe value.  An Unsafe value combined with anything should be
-- considered unsafe.
--
-- It's easy enough to define the safe unsafe types.  But the issue is
-- that all our existing code expects raw strings or ints or whatever,
-- and now we have to painstakingly do coversions (and of course if we
-- get those conversions wrong we'll violate the safety priciples).

-- Luckily, this idea of safety naturally fits into the context of an
-- applicative functor.  Read this:

-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors

-- If we can implement the applicative functor typeclass, and if we do
-- that we can use the liftA functions to convert our boring functions
-- into safety respecting functions.

--I'm implementing the fuctor typeclass, I'll leave you to do the
--applicative typeclass

instance Functor MaybeSafe where
  fmap f (Safe a) = Safe (f a)
  fmap f (Unsafe a) = Unsafe (f a)

-- test code below

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual
  | expected == actual = putStrLn (testName ++ " PASSED")
  | otherwise =
    do
      putStrLn $ testName ++ " FAILED"
      putStrLn $ "Expected: " ++ (show expected)
      putStrLn $ "Actual: " ++ (show actual)

main= let (<+>) = liftA2 (+)
          (<->) = liftA2 (-) in
  do
    assertEqual "safe+unsafe" (Unsafe (-1)) (Safe 2 <-> Unsafe 3)
    assertEqual "safe+safe" (Safe 5) (Safe 2 <+> Safe 3)
    assertEqual "unsafe+unsafe" (Unsafe 9) (Unsafe 2 <+> Unsafe 7)
    assertEqual "unsafe+safe" (Unsafe (-5)) (Unsafe 2 <-> Safe 7)
    assertEqual "using strings" (Unsafe "foo") ((liftA2 (++)) (Safe "f") (Unsafe "oo"))

