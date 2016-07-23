module EditUserInformation exposing (..)
import Location exposing (Location)
import Html.App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (id, list, href, placeholder)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
import Http
import Json.Encode as Encode
import Task exposing (Task)
import Json.Decode as Decode
import InputLocation
import Json.Decode as Json exposing ((:=))


type alias Model = (List Location)


encoder : Location.Location -> Encode.Value
encoder {name,country,city,postal} =
  Encode.object
    [("name", Encode.string name)
    ,("country", Encode.string country)
    ,("city", Encode.string city)
    ,("postal", Encode.string postal)
    ]


addME : Int -> Platform.Task Http.Error (List Location)
addME userId =
  (encoder Location.empty)
    |> Encode.encode 0
    |> Http.string
    |> post'
      (Decode.list Location.decoder) ("/api/" ++ toString userId ++ "/postLocation" )


type Msg
  = AddME
  | AddFailed Http.Error
  | AddComplete (List Location)
  | Modify Int InputLocation.Msg

modifyHelp : Int -> InputLocation.Msg -> Location -> ( Maybe Location, Cmd Msg)
modifyHelp targetId msg location =

  if location.id /= targetId then
    ( Just location, Cmd.none )
  else
    let
      ( maybeNewLocation, cmd ) =
        InputLocation.update msg location
    in
      ( maybeNewLocation
      , Cmd.map (Modify targetId) cmd
      )

update : Msg -> Int -> Model -> (Model, Cmd Msg)
update msg userId locations =
  case msg of
    Modify givenId msg ->
      let
        ( initialLocations, cmds ) =
            List.unzip (List.map (modifyHelp givenId msg) locations)

        newLocations =
          List.filterMap identity initialLocations
      in
        ( newLocations
        , Cmd.batch cmds)
    AddME ->
      ( locations
      , Task.perform AddFailed AddComplete ( addME (Debug.log "adding" userId)))
    AddFailed _ ->
      ( locations, Debug.log "failed to add" Cmd.none)
    AddComplete addedLocation ->
      let
        newLocations =
          ( List.append addedLocation locations )
      in
        ( newLocations
        , Debug.log "completed add" Cmd.none
        )

htmlToList: Html b -> List (Html b)
htmlToList a =
  flip (::) [] a

view : Model -> Html Msg
view model =
  div
    [ id "header" ]
    [ h2
      []
      [ text "edit user (location) information" ]
  , div
      [ id "addLocationFields" ]
      [ text "Constituent Of: " ]
  , div
      [ id "addLocation" ]
      [ button
        [ onClick AddME ]
        [ text "Add new location" ]
      ]
  , div
      [ id "currentLocations" ]
      (List.map viewCurrentLocations model)
  ]
viewCurrentLocations: InputLocation.Model -> Html Msg
viewCurrentLocations location =
  Html.App.map ( Modify location.id ) (InputLocation.view location)

post' : Decode.Decoder a -> String -> Http.Body -> Task.Task Http.Error a
post' decoder url body =
  Http.send Http.defaultSettings
    { verb = "POST"
    , headers = [("Content-type", "application/json")]
    , url = url
    , body = body
    }
  |> Http.fromJson decoder
