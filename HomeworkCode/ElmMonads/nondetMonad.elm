import Html exposing (text)
import Set exposing (..)
import List

{-
This is one of two monads I'm going to ask you to write in Elm.
It is worth 15/30 points for this assignment.

This one is for nondeterminstic integer operations.  Basically, what
we want to to write integer functions - but these functions might
return 0 or many results.  We want to be able to combine these
functions and have them try all possible values.

The particular use case I have in mind is an game you may have played
in math class as a kid.  Given 3 integers, you were asked to come up
with a combination of addition subtract multiplication and division
that would reach some target number.  i.e. if the numbers are 1 2 and 3.

(1*2)+3 yields 5
(1-2)*3 yields -3

etc.

In the case we won't consider the possibility that different orderings
e.g. (2*3)+1 or even orderinsg on the same operation e.g. 1-(2*3) can
generate additional results.

In terms of a monad, what we want in a monad that is a list/set of
values.  Operations will take integers but will return monadic values
of 0, 1 or many results.  These will be combined by the >>= operator,
which will take each of the results of the previous stage, apply them
individually to the next stage, and the union all the various results
into one giant result.

-}

-- I used a set, but feel free to use a list if you prefer. This way I
-- didn't have to think about uniquifying or sorting my results.
type alias NotDet = Set Int

-- you'll want a return function, but I leave the type signature up to you

-- here's an example operation
-- multiply : Int -> Int -> NotDet
-- multiply x y = your cool code here

-- divide is interesting
-- in this case, becase we're talking integers
--
-- if x/y yields an integer, we should return a monadic value
-- representing the result
--
-- if x/y yeilds a non-integer, we should return a monadic value
-- representing an empty result

-- divide : Int -> Int -> NotDet

-- add and subtract should work similarly.  If you want, make a handy
-- function so that add, subtract, and multiply don't have such
-- boilerplate code


-- do anything.  This is the non-derministic combination of the 4
-- other operations.
-- so it should return a monadic x+y,x-y,x*y,x/y
-- doAnything : Int -> Int -> NotDet

-- I'll leave the signature of the monad >>= operator to you
-- see the randomness example for a hint

-- Here's my final code that uses the operations.  This should work
-- with your version too!

-- getAllCombinations a b c =
--  doAnything a b >>= (\result ->
--  doAnything result c)

-- main = text (toString (getAllCombinations 1 2 3))
