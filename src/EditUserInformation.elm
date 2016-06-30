module EditUserInformation exposing (..)
import Account
import Location
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (id, list, href, placeholder)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
--import Task exposing (Task)
import Http
--import String
import Json.Encode as Encode



type alias Model =
  { name: String
  , country: String
  , city: String
  , postal: String
  , updateError: Maybe Http.Error
  }


init : Model
init =
  { name = ""
  , country = ""
  , city = ""
  , postal = ""
  , updateError = Nothing
  }

encoder : Model -> Encode.Value
encoder {name, country, city, postal} =
  Encode.object
    [("name", Encode.string name)
    ,("country", Encode.string country)
    ,("city", Encode.string city)
    ,("postal", Encode.string postal)
    ]

type Msg
  = ChangeName String
  |ChangeCo String
  |ChangeCi String
  |ChangePo String
  |UpdateME
  |UpdateFailed Http.Error
  |UpdateComplete

updateMe : Model -> Account.User -> Platform.Task Http.Error Location.Location
updateMe model user =
  (encoder model)
    |> Encode.encode 0
    |> Http.string
    |> Http.post
      Location.decoder ("/api/" ++ toString user.id ++ "/updateLocation" )

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeName input ->
      { model | name = ( Debug.log "input" input) }
    ChangeCo input ->
      { model | country = ( Debug.log "input" input) }
    ChangeCi input ->
      { model | city = ( Debug.log "input" input) }
    ChangePo input ->
      { model | postal = ( Debug.log "input" input) }
    UpdateME ->
      (model) --Task.perform UpdateFailed UpdateComplete ( updateMe (Debug.log "updating") model ))
    UpdateFailed err ->
      ({ model | updateError = Just ( Debug.log "failedUpdate" err) })
    UpdateComplete ->
      (model)
view : Model -> Html Msg
view model =
  div [ id "header" ]
    [ h2 []
      [text "placeholder"]
  , div [ id "locationInputs"]
    [ h2 [] [ text "Constituent Of: "]
      , input [ placeholder "Location Name", onInput ChangeName ] []
      , input [ placeholder "Country Name", onInput ChangeCo ] []
      , input [ placeholder "City Name", onInput ChangeCi ] []
      , input [ placeholder "Postal Number", onInput ChangePo ] []
    ]
  , div [ id "locationButton"]
    [ button [ onClick UpdateME ] [text "UPDATE YOUR LOCATION"]]
  , div [][text model.country]
  ]
