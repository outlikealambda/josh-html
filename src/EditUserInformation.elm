module EditUserInformation exposing (..)
import Account
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

type alias Model =
  {user: Account.User
  , status: String
  , name: String
  , country: String
  , city: String
  , postal: String
  , currentLocations: List InputLocation.Model
  }

init : Account.User -> Model
init user =
  {user = user
  , status = "initial"
  , name = ""
  , country = ""
  , city = ""
  , postal = ""
  , currentLocations = user.locations
  }

encoder : Model -> Encode.Value
encoder model =
  Encode.object
    [("name", Encode.string model.name)
    ,("country", Encode.string model.country)
    ,("city", Encode.string model.city)
    ,("postal", Encode.string model.postal)
    ]

addME : Model -> Platform.Task Http.Error Location.Location
addME model =
  (encoder model)
    |> Encode.encode 0
    |> Http.string
    |> post'
      Location.decoder ("/api/" ++ toString model.user.id ++ "/postLocation" )


type Msg
  =AddME
  |AddFailed Http.Error
  |AddComplete Location
  |ChangeName String
  |ChangeCo String
  |ChangeCi String
  |ChangePo String
  |Modify Int InputLocation.Msg

modifyHelp : Int -> InputLocation.Msg -> Location -> (Location, Cmd Msg)
modifyHelp targetId msg location =
  if location.id /= targetId then
    ( location, Cmd.none )
  else
    let
      ( newLocation, cmds ) =
        InputLocation.update msg location
    in
      ( newLocation
      , Cmd.map (Modify targetId) cmds
      )

update : Msg -> Account.User -> Model -> (Model, Cmd Msg)
update msg user model =
  case msg of
    Modify givenId msg ->
      let
        (newLocations, cmd) =
            List.unzip (List.map (modifyHelp givenId msg) model.currentLocations)
      in
        ({model
        | currentLocations = newLocations}
        , Cmd.batch cmd)
    ChangeName input ->
      { model | name = ( Debug.log "input" input) } ![]
    ChangeCo input ->
      { model | country = ( Debug.log "input" input) } ![]
    ChangeCi input ->
      { model | city = ( Debug.log "input" input) } ![]
    ChangePo input ->
      { model | postal = ( Debug.log "input" input) } ![]
    AddME ->
      ({model
      | status = "currently adding"}
      , Task.perform AddFailed AddComplete ( addME (Debug.log "adding" model)))
    AddFailed _ ->
      { model | status = "failed to add"} ![]
    AddComplete addedLocation ->
      {model
      | status = "add completed"
      , name = ""
      , country = ""
      , city = ""
      , postal = ""
      , user = Account.listToUser (Account.addLocation user addedLocation) user
      } ![]


htmlToList: Html b -> List (Html b)
htmlToList a =
  flip (::) [] a

view : Model -> Html Msg
view model =
  div [ id "header" ]
    [ h2 []
      [text "edit user (location) information"]
  , div [id "homeButton"]
      [ button
        [ ]
        [text "This button doesn't do anything"]
      ]
  , div [id "addLocationFields"]
      [ text "Constituent Of: "]
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
  , div [ id "addLocation"]
    [ button
      [onClick AddME]
      [text "Add new location"]
    ]
  , div [ id "currentLocations"]
        (List.map viewCurrentLocations model.currentLocations)
    ]
viewCurrentLocations: InputLocation.Model -> Html Msg
viewCurrentLocations location =
  Html.App.map ( Modify location.id ) (InputLocation.view location)

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
