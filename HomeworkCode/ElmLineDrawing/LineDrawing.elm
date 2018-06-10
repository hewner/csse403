import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Signal exposing (..)
import Window
import Keyboard


{- Before you start, it might be a good idea to reread the foldp
discussion here, plus the notes:

http://package.elm-lang.org/packages/elm-lang/core/1.1.0/Signal

Here's some code that displays a line.

STAGE 1 [15 Points]:

Modify this code so that by clicking, you can draw starting at the
center point, then from the center point to the first click, from the
first click to the 2nd click, etc.

STAGE 2 [10 Points]

Make it so that if you hit spacebar it resets your drawing.  You go
back to drawing from the center point, removing any other lines you
have drawn.

-}

main : Signal Element
main =
  map scene Mouse.position

localPoint (x,y) = (toFloat x - toFloat 500 / 2, toFloat 500 / 2 - toFloat y)

scene : (Int,Int) -> Element
scene (x,y) =
      collage 500 500
                  [ traced defaultLine (path [(0,0),localPoint (x,y)]) ]

-- hint: to get a input stream that gives you coordinates on a click,
-- you might want to try something like this:

positionOnClickSignal = sampleOn Mouse.clicks Mouse.position

-- look up sampleOn in the Signals documentation
