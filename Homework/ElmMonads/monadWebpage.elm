import Html exposing (Html, beginnerProgram, div, button, text)
import Html.Events exposing (onClick)
import List exposing (append)

type alias Model = Int

newButton buttonText message = 
  button [ onClick message ] [ text buttonText ]

newLabel labelText = text labelText

type alias WebpageMonadicType = List (List (Html Msg))

emptyPage : WebpageMonadicType
emptyPage = [[]]

add: Html Msg -> WebpageMonadicType -> WebpageMonadicType
add html monadVal =
  case monadVal of
    h::tail -> (append h [html]) :: tail
    _ -> monadVal

divStart: WebpageMonadicType -> WebpageMonadicType
divStart monadVal = []::monadVal

divEnd: WebpageMonadicType -> WebpageMonadicType
divEnd monadVal =
  case monadVal of
    h1::h2::tail -> append h2 [div [] h1] :: tail
    _ -> monadVal

toHtml : WebpageMonadicType -> Html Msg
toHtml monadVal =
  case monadVal of
    [[html]] -> html
    _ -> text "Error"
    
    
(>>=): WebpageMonadicType -> (WebpageMonadicType -> WebpageMonadicType) -> WebpageMonadicType
(>>=) monadicValue function =
  function monadicValue


main =
  beginnerProgram { model = 0, view = view, update = update }


view : Model -> Html Msg
view model =
  toHtml 
  ( 
    emptyPage >>=
    divStart >>=
    add (newButton "-" Decrement ) >>=
    divStart >>=
    add (newLabel (toString model)) >>=
    divEnd >>=
    add (newButton "+" Increment ) >>=
    divEnd
  )


type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1
