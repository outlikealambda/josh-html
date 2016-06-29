module UpdateLocation exposing (..)
import Account exposing (Account, Location, accountDecoder, locationDecoder)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (id, list, href, placeholder)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
--import Task exposing (Task)
--import Http


type alias Model =
  { header : String
  , inputName: String
  , inputCountry: String
  , inputCity: String
  , inputPostal: String
  , user: Account
  }

init : Account -> Model
init account =
  { header = "Update your location here"
  , user = account
  , inputName = ""
  , inputCountry = ""
  , inputCity = ""
  , inputPostal = ""
  }

type Msg
  = ChangeName String
  |ChangeCo String
  |ChangeCi String
  |ChangePo String
  --|UpdateME
  --|UpdateFailed
  --|UpdateComplete

{-updateMe : Model -> Task Http.Error Location
updateMe model =
  Http.post locationDecoder("/api/" ++ model.user.id ++ "/" ++ model.user.location.id ++ "/updateLocation" )
-}
update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeName input ->
      { model | inputName = ( Debug.log "input" input) }
    ChangeCo input ->
      { model | inputCountry = ( Debug.log "input" input) }
    ChangeCi input ->
      { model | inputCity = ( Debug.log "input" input) }
    ChangePo input ->
      { model | inputPostal = ( Debug.log "input" input) }
    {-UpdateME ->
      (model, Task.perform UpdateFailed UpdateComplete ( updateMe (Debug.log "updating" )))
-}
view : Model -> Html Msg
view model =
  div [ id "header" ]
    [ h2 []
      [text model.header]
  , div [ id "locationInputs"]
    [ h2 [] [ text "Constituent Of: "]
      , input [ placeholder "Location Name", onInput ChangeName ] []
      , input [ placeholder "Country Name", onInput ChangeCo ] []
      , input [ placeholder "City Name", onInput ChangeCi ] []
      , input [ placeholder "Postal Number", onInput ChangePo ] []
      --, button [ onClick UpdateME ] [text "UPDATE YOUR LOCATION"]
    ]
  , div [][text model.inputCountry]
  ]
