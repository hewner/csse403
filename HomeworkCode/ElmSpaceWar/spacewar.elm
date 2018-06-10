import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Signal exposing (..)
import Window
import List
import Time
import Debug
import Keyboard
import Transform2D    

{-
SpaceWar Assignment! (35 points in total)

Requirements: 

[7 points] 1.  Two ships which rotate with a/d or left/right arrows
respectively

[8 points] 2.  w/s or up/down should cause them to accelerate in the direction
they are facing (the system should have inertia)

[10 points] 3.  ctrl or enter should cause them to fire a bullet in the direction
they are facing

[10 points] 4.  A ship getting hit by a bullet should cause the game to end in
some way - could be hit ship turns red and is motionless, could be
screen changes to "Player X wins".  It should just be clear the game
is over and who won.

If you're curious/having fun, check out all the features the original
spacewar had.  Teleporting, gravity, angular momentum, etc.

http://en.wikipedia.org/wiki/Spacewar_%28video_game%29

-}
    
drawBullet = filled green (circle 5)

drawShip : Color -> Form
drawShip color =
    let triangle = filled color (ngon 3 10)
    in group [moveX -5 triangle, moveX 5 triangle]
       
main = collage 800 800 [drawShip orange]
