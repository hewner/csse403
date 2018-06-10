import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Mouse
import Signal (..)
import Text (asText)
import Window
import List
import Time
import Debug
import Keyboard
import Transform2D    

type alias Object = {x:Float, y:Float, vx:Float, vy:Float}
type BUpdater = BUpdater (Float,Float) (Float -> BUpdater) 
type Event = Bullet Int | Tick {delta:Float,p1arrow : {x:Int,y:Int},p2arrow : {x:Int,y:Int}}
    
ship1 = {x=1,y=1,vx=0,vy=0}
startingObjects : {p1:Object, bullets:List BUpdater}
startingObjects = {p1=ship1, bullets=[]}

stepShip {x,y} bullets delta ship =
    {ship | x <- ship.x + (toFloat x*4)*delta, y <- ship.y + (toFloat y*4)*delta}

--- The Interesting Part Starts Here
    
straightBulletCreate x y =
    straightBulletUpdate (x+10) y 0

straightBulletUpdate : Float -> Float -> Float -> BUpdater
straightBulletUpdate x y delta =
    let newX = x + delta*4
        newY = y
    in BUpdater (newX, newY) (straightBulletUpdate newX newY)

sineBulletCreate x y =
    sineBulletUpdate (x+10) y (x+10) y 0
    
sineBulletUpdate x y initialX initialY delta =
    let newX = x + delta*3
        deltaX = newX - initialX
        newY = initialY + 50*sin (deltaX/15)
    in BUpdater (newX, newY) (sineBulletUpdate newX newY initialX initialY)   

mineBulletCreate x y =
    mineBulletUpdate (x-10) (x-50) y 0
           
mineBulletUpdate x targetX y delta = 
    let newX = x - delta*4
        newY = y
    in if(x > targetX) then
           BUpdater (newX, newY) (mineBulletUpdate newX targetX newY)
       else
           doNothingUpdate newX newY delta

doNothingUpdate x y delta = BUpdater (x,y) (doNothingUpdate x y)
        
doBUpdate delta bupdater =
    case bupdater of
      BUpdater coord updateFunc -> updateFunc delta

getBUpdateCoords bupdater =
    case bupdater of
      BUpdater (x,y) updateFunc -> {x=x,y=y,vx=0,vy=0}
               
createBullet ship update =
    update ship.x ship.y

           
    
step event list = case event of 
           Tick input -> {p1=stepShip input.p1arrow list.bullets (input.delta/20) list.p1,
                          bullets = List.map (doBUpdate (input.delta/20)) list.bullets }
           Bullet 1 -> {list|bullets <- [createBullet list.p1 straightBulletCreate] ++ list.bullets}
           Bullet 2 -> {list|bullets <- [createBullet list.p1 sineBulletCreate] ++ list.bullets}

--- Mostly the stuff below here you've seen before

state = foldp step startingObjects allinputs

allinputs : Signal Event
allinputs = let timer = Time.fps 60
                tickinputs = map3 (\wasd arrows tdelta-> Tick {p1arrow = wasd, p2arrow=arrows,delta=tdelta}) Keyboard.wasd Keyboard.arrows timer
                timedInputs = sampleOn timer tickinputs
                fire1 = map (\anything -> Bullet 1) Keyboard.ctrl
                fire2 = map (\anything -> Bullet 2) Keyboard.enter
            in mergeMany [timedInputs,fire1,fire2]

drawBullet = filled green (circle 5)                  
              
drawShip =
    let triangle = filled blue (ngon 3 10)
    in group [moveX -5 triangle, moveX 5 triangle]
       
render drawFunc {x,y} = move (x,y) drawFunc
                       
renderall objs = collage 800 800
                 ([render drawShip objs.p1]
                 ++ (List.map (render drawBullet) (List.map getBUpdateCoords objs.bullets)))
    
main :  Signal Element
main =
    map renderall state




