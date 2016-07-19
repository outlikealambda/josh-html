module InputLocation exposing (..)
import Location exposing (Location)
import Account
import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Html.Events exposing (onClick, onInput)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
import Html.Attributes exposing (id, list, href, placeholder)
import Task exposing (Task)

type alias Model = Location

type Msg
  =ChangeName String
  |ChangeCo String
  |ChangeCi String
  |ChangePo String
  |UpdateLocation
  |UpdateFailed Http.Error
  |UpdateComplete Location
  |RemoveLocation
  |RemoveFailed Http.Error
  |RemoveComplete Account.User



encoder : Model -> Encode.Value
encoder model =
  Encode.object
    [("name", Encode.string model.name)
    ,("country", Encode.string model.country)
    ,("city", Encode.string model.city)
    ,("postal", Encode.string model.postal)
    ]


updateMe : Model -> Platform.Task Http.Error Location
updateMe model =

  (encoder model)
    |> Encode.encode 0
    |> Http.string
    |> post'
      Location.decoder ("/api/" ++ toString model.id ++ "/updateLocation" )


removeME : Model -> Platform.Task Http.Error Account.User
removeME model =
  delete'
    Account.decoder
    ("/api/" ++ toString model.id ++ "/deleteLocation" )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeName input ->
      { model | name = ( Debug.log "input" input) } ![]
    ChangeCo input ->
      { model | country = ( Debug.log "input" input) } ![]
    ChangeCi input ->
      { model | city = ( Debug.log "input" input) } ![]
    ChangePo input ->
      { model | postal = ( Debug.log "input" input) } ![]
    UpdateLocation ->
      (model
      , Task.perform UpdateFailed UpdateComplete ( updateMe (Debug.log "updating" model)))
    UpdateFailed _ ->
      model ![]
    UpdateComplete {name, id, country, city, postal} ->
      {model
      | name = name
      , id = id
      , country = country
      , city = city
      , postal = postal
      } ![]
    RemoveLocation ->
      (model
      , Task.perform RemoveFailed RemoveComplete ( removeME (Debug.log "removing" model)))
    RemoveFailed _ ->
      model ![]
    RemoveComplete _ ->
      model ![]

view : Model -> Html Msg
view model =
  div [ id "locationInputs"]
    [ h2 []
      []
        , input
            [ placeholder "Location Name"
            , onInput ChangeName
            , Html.Attributes.value model.name
            ]
            []
        , input
            [ placeholder "Country Name"
            , onInput ChangeCo
            , Html.Attributes.value model.country
            ]
            []
        , input
            [ placeholder "City Name"
            , onInput ChangeCi
            , Html.Attributes.value model.city
            ]
            []
        , input
            [ placeholder "Postal Number"
            , onInput ChangePo
            , Html.Attributes.value model.postal
            ]
            []
  , div [ id "updateLocation"]
    [ button
      [ onClick UpdateLocation ]
      [text "CHANGE THIS LOCATION"]
    ]
  , div [ id "deleteLocation"]
    [ button
      [onClick RemoveLocation]
      [text "REMOVE THIS LOCATION"]
    ]
    ]

delete' : Decode.Decoder a -> String ->Task.Task Http.Error a
delete' decoder url =
  Http.send Http.defaultSettings
    { verb = "DELETE"
    , headers = [("Content-type", "application/json")]
    , url = url
    , body = Http.empty
    }
  |> Http.fromJson decoder

post' : Decode.Decoder a -> String -> Http.Body -> Task.Task Http.Error a
post' decoder url body =
  Http.send Http.defaultSettings
    { verb = "POST"
    , headers = [("Content-type", "application/json")]
    , url = url
    , body = body
    }
  |> Http.fromJson decoder
