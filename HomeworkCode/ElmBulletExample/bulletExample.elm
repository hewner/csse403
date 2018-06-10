import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type BUpdater = BUpdater (Float,Float) (Float -> BUpdater) 
type alias Model = {pos: List BUpdater, lastTime : Maybe Time}


init : (Model, Cmd Msg)
init =
  ({pos=[straightBulletUpdate 30 30 0, sineBulletCreate 30 50], lastTime=Nothing}, Cmd.none)


-- mineBulletUpdate 30 60 80 0

straightBulletUpdate : Float -> Float -> Float -> BUpdater
straightBulletUpdate x y delta =
    let newX = x + delta/50
        newY = y
    in BUpdater (newX, newY) (straightBulletUpdate newX newY)


sineBulletCreate x y =
    sineBulletUpdate (x+10) y (x+10) y 0

sineBulletUpdate x y initialX initialY delta =
    let newX = x + delta/100
        deltaX = newX - initialX
        newY = initialY + 15*sin (deltaX/3)
    in BUpdater (newX, newY) (sineBulletUpdate newX newY initialX initialY)

mineBulletUpdate x targetX y delta = 
    let newX = x + delta/50
        newY = y
    in if(x < targetX) then
           BUpdater (newX, newY) (mineBulletUpdate newX targetX newY)
       else
           doNothingUpdate newX newY delta

doNothingUpdate x y delta = BUpdater (x,y) (doNothingUpdate x y)

-- UPDATE

type Msg
  = Tick Time

updatePos : Float -> BUpdater -> BUpdater
updatePos deltaT updater = 
 case updater of
   BUpdater _ func -> func deltaT

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      case model.lastTime of
        Just oldTime -> ({model|lastTime=Just newTime, 
                                pos=List.map (updatePos (newTime - oldTime)) model.pos}, Cmd.none)
        Nothing -> ({model|lastTime=Just newTime}, Cmd.none)


-- SUBSCRIPTIONS


----Note that in your game you problably want to use the AnimationFrame package
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.millisecond*40) Tick



-- VIEW

posToBullet: BUpdater -> Svg Msg
posToBullet updater =
 case updater of
   BUpdater (x,y) _ -> circle [ cx (toString x), cy (toString y), r "3", fill "#0B79CE" ] []

view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 100 100", width "300px" ]
      (List.map posToBullet model.pos)
