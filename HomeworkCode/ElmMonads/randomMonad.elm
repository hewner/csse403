import Html exposing (text)
import Random exposing (..)

{-
This is one of two monads I'm going to ask you to write in Elm.
It is worth 15/30 points for this assignment.

This one is generating random numbers.  It's annoying to have the
random seed around everywhere - we want a function that will save us
the agony.  Here's the result in action:

doCode = 
     return "Random numbers are " >>= (\prevResult1 ->
     getRandom >>= (\prevResult2 ->
     getRandom >>= (\prevResult3 ->
     getRandom >>= (\prevResult4 ->
     return (prevResult1 ++ (toString prevResult2) ++ " " ++ (toString prevResult3) ++ " " ++ (toString prevResult4)

     )))))

The basic idea is that our monadic data value is a function that takes
a seed as a paramter and returns a pair of (returnValue, newSeed)

type alias RandomState value = Seed -> (value, Seed)

Ordinary code just keeps the seed the same and returns the result of
the operation.

return : value -> RandomState value
return x = (\seed -> (x, seed))

getRandom however returns a new random float bewteen 0 and 1 based on
the seed and returns a pair of the random value and the new seed.

getRandom : RandomState Float
I'll leave this implementation to you

The combination operatior takes a function that takes a seed and
returns an output + seed, it also takes a function that takes the
output value, and returns a function that takes a seed a returns a
maybe different output value.

The basic idea here is you want to return a new function.  It will
take a seed as a parameter.  It will pass the seed to the first
function, get a new seed and an output value.  Then it will pass the
output value to the second parameter, getting a function that takes a
seed and returns a new seed and output.  Give your new seed to that
returned function, and return its output.

(>>=): RandomState value -> (value -> RandomState newval) -> RandomState newval

-}


random : Generator Float
random = float 0 1


type alias RandomState value = Seed -> (value, Seed)

return : value -> RandomState value
return x = (\seed -> (x, seed))

-- getRandom : RandomState Float
-- getRandom = YOUR CODE HERE

-- (>>=): RandomState value -> (value -> RandomState newval) -> RandomState newval
-- (>>=) state next = YOUR CODE HERE


{-

Here's the code using the randomMonad in action.  This should work on yours if you code things correctly.

doCode = 
     return "Random numbers are " >>= (\prevResult1 ->
     getRandom >>= (\prevResult2 ->
     getRandom >>= (\prevResult3 ->
     getRandom >>= (\prevResult4 ->
     return (prevResult1 ++ (toString prevResult2) ++ " " ++ (toString prevResult3) ++ " " ++ (toString prevResult4)

     )))))

main = let (output,seed) = doCode (initialSeed 2) in text (toString output)

-}
