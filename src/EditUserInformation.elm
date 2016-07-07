module EditUserInformation exposing (..)
import Account
import Location exposing (Location)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (id, list, href, placeholder)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
import Http
import Json.Encode as Encode
import Task exposing (Task)
import Json.Decode as Decode





type alias Model =
  { name: String
  , country: String
  , city: String
  , postal: String
  , getError: Maybe Http.Error
  , updateError: Maybe Http.Error
  , addError: Maybe Http.Error
  , removeError: Maybe Http.Error
  , status: String
  }


init : Account.User -> (Model, Cmd Msg)
init user =
  (
  { name = ""
  , country = ""
  , city = ""
  , postal = ""
  , getError = Nothing
  , updateError = Nothing
  , addError = Nothing
  , removeError = Nothing
  , status = ""
  }, getCmd user
  )

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
  |GetME
  |GetFailed Http.Error
  |GetComplete Location
  |UpdateME
  |UpdateFailed Http.Error
  |UpdateComplete Location
{-
  |AddME
  |AddFailed Http.Error
  |AddComplete Location
-}
  |RemoveME
  |RemoveFailed Http.Error
  |RemoveComplete Account.User

getCmd : Account.User -> Cmd Msg
getCmd user =
   Task.perform GetFailed GetComplete (getME user)

getME : Account.User -> Platform.Task Http.Error Location.Location
getME user =
  Http.get
  Location.decoder ("/api/" ++ toString user.id ++ "/getLocation")

updateMe : Model -> Account.User -> Platform.Task Http.Error Location.Location
updateMe model user =

  (encoder model)
    |> Encode.encode 0
    |> Http.string
    |> post'
      Location.decoder ("/api/" ++ toString user.id ++ "/updateLocation" )
{-
addME : Model -> Account.User -> Platform.Task Http.Error Location.Location
addME model user =

  (encoder model)
    |> Encode.encode 0
    |> Http.string
    |> post'
      Location.decoder ("/api/" ++ toString user.id ++ "/postLocation" )
-}

removeME : Account.User -> Platform.Task Http.Error Account.User
removeME user =

  delete'
    Account.decoder
    ("/api/" ++ toString user.id ++ "/deleteLocation" )


update : Msg -> Account.User -> Model -> (Model, Cmd Msg)
update msg user model =
  case msg of
    ChangeName input ->
      { model | name = ( Debug.log "input" input) } ![]
    ChangeCo input ->
      { model | country = ( Debug.log "input" input) } ![]
    ChangeCi input ->
      { model | city = ( Debug.log "input" input) } ![]
    ChangePo input ->
      { model | postal = ( Debug.log "input" input) } ![]
    GetME ->
      ({model | status = "getting"}, Task.perform GetFailed GetComplete ( getME (Debug.log "getting" user)))
    GetFailed err ->
      { model | getError = Just ( Debug.log "failedGet" err)}![]
    GetComplete {name, country, city, postal} ->
      {model | status = "got", name = name, country = country, city = city, postal = postal }![]
    UpdateME ->
      ({model | status = "updating"}, Task.perform UpdateFailed UpdateComplete ( updateMe (Debug.log "updating" model) user ))
    UpdateFailed err ->
      { model | updateError = Just ( Debug.log "failedUpdate" err) }![]
    UpdateComplete updatedLocation ->
      {model | status = "updated"} ![]
{-
    AddME ->
      ({model | status = "adding"}, Task.perform AddFailed AddComplete ( addME (Debug.log "adding" model) user ))
    AddFailed err ->
      { model | addError = Just ( Debug.log "failedAdd" err) }![]
    AddComplete addedLocation ->
      {model | status = "added"} ![]
-}
    RemoveME ->
      ({model | status = "removing"}, Task.perform RemoveFailed RemoveComplete ( removeME (Debug.log "removing" user)))
    RemoveFailed err ->
      { model | removeError = Just ( Debug.log "failedRemove" err) }![]
    RemoveComplete currentUser ->
      {model | status = "removed"} ![]



view : Model -> Html Msg
view model =
  div [ id "header" ]
    [ h2 []
      [text "edit user (location) information"]
  , div [ id "locationInputs"]
    [ h2 [] [ text "Constituent Of: "]
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
    ]
  , div [ id "updateLocation"]
    [ button [ onClick UpdateME ] [text "CHANGE YOUR LOCATION"]]
{-
  , div [ id "addLocation"]
    [ button [onClick AddME][text "ADD YOUR LOCATION"]]
-}
  , div [ id "deleteLocation"]
    [ button [onClick RemoveME][text "REMOVE YOUR LOCATION"]]

  , div [][text model.status]
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
