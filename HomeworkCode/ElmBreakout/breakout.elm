---
type: text/plain
---

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard
import Char
import List


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL



type alias Model = {x : Int, y : Int, key : ButtonState}

init : (Model, Cmd Msg)
init =
  ({x=30,
    y=440,
    key=None}, Cmd.none)



-- UPDATE


type Msg
  = Key ButtonState

type ButtonState
  = Left | Right | None | Both


    
keyToVel key =
    case key of
        Left -> -5
        Right -> 5
        _ -> 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key q -> ({model|key=q, x = model.x + keyToVel q}, Cmd.none)
      
-- SUBSCRIPTIONS
-- 37 is left
-- 39 is right
handleDown : (Keyboard.KeyCode, ButtonState) -> Msg
handleDown state =
    case state of
        (37, Right) -> Key Both
        (39, Left) -> Key Both
        (37, None) -> Key Left
        (39, None) -> Key Right
        (_ , current) -> Key current

handleUp : (Keyboard.KeyCode, ButtonState) -> Msg
handleUp state =
    case state of
        (37, Both) -> Key Right
        (39, Both) -> Key Left
        (37, Left) -> Key None
        (39, Right) -> Key None
        (_ , current) -> Key current


-- on my browser keyboard presses does not work properly
-- so I'm using ups and downs
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
      Keyboard.downs (\k -> handleDown (k, model.key))
      , Keyboard.ups (\k -> handleUp (k, model.key))
     ]



-- VIEW


view : Model -> Html Msg
view model = 
   svg [ viewBox "0 0 500 500", width "500px" ]
       [ rect [ x (toString model.x), y (toString model.y), width "60", height "10", fill "#0B79CE" ] []
       ]


