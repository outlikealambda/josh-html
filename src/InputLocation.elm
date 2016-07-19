module InputLocation exposing (..)
import Location exposing (Location)
import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode as Json exposing ((:=))
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
  |RemoveComplete Int



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


removeME : Model -> Platform.Task Http.Error Int
removeME model =
  delete'
    removalDecoder
    ("/api/" ++ toString model.id ++ "/deleteLocation" )

removalDecoder : Json.Decoder Int
removalDecoder =
  Json.int

update : Msg -> Model -> (Maybe Model, Cmd Msg)
update msg model =
  case msg of
    ChangeName input ->
      Just { model | name = ( Debug.log "input" input) } ![]
    ChangeCo input ->
      Just { model | country = ( Debug.log "input" input) } ![]
    ChangeCi input ->
      Just { model | city = ( Debug.log "input" input) } ![]
    ChangePo input ->
      Just { model | postal = ( Debug.log "input" input) } ![]
    UpdateLocation ->
      (Just model
      , Task.perform UpdateFailed UpdateComplete ( updateMe (Debug.log "updating" model)))
    UpdateFailed _ ->
      (Just model, Debug.log "failed to update" Cmd.none)
    UpdateComplete {name,id,country,city,postal} ->

      (Just {model
      | name = name
      , id = id
      , country = country
      , city = city
      , postal = postal
      }, Debug.log "completed update" Cmd.none)
    RemoveLocation ->
      (Just model
      , Task.perform RemoveFailed RemoveComplete ( removeME (Debug.log "removing" model)))
    RemoveFailed _ ->
      (Just model, Debug.log "failed to remove" Cmd.none)
    RemoveComplete locationId ->
      (Nothing, Debug.log "completed removal" Cmd.none)

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
