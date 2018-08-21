import Html exposing (text)
import List exposing (tail, head)

-- this one is just a completed example that we discussed in class

mylist1 = [1, 2, 3]
mylist2 = [1]

(>>=): Maybe a -> (a -> Maybe b) -> (Maybe b)
(>>=) maybeVal function =
  case maybeVal of
    Nothing -> Nothing
    Just val -> function val

thirdElement inputList = 
  tail inputList >>= (\result1 -> 
  tail result1 >>= (\result2 -> 
  head result2))

main = text (toString (thirdElement mylist1))
