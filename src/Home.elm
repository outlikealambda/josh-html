module Home exposing (..)
import Html.App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (id, list, href, placeholder)
import Html exposing (span, text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
import Http
import Task exposing (Task)
import EditUserInformation
import Account
import Location exposing (Location)



main : Program Never
main =
  Html.App.program {init = init, view = view, update = update, subscriptions = subscriptions'}

type alias Model =
  { fetchError : Maybe Http.Error
  , userIdInput : String
  , header : String
  , user : Account.User
  , page: Page
  , inputLocation : EditUserInformation.Model
  }

init : (Model, Cmd Msg)
init =
  let
    user =
        { name = ""
        , id = -1
        , emails = []
        , locations = []
        }
    inputLocation =
      EditUserInformation.init user
  in
    ({page = Home
    , fetchError = Nothing
    , header = "User Information"
    , userIdInput = ""
    , user = user
    , inputLocation = inputLocation
    }, Cmd.map UpdateLocationMsg Cmd.none)--Cmd.map UpdateLocationMsg msg)

subscriptions' : Model -> Sub Msg
subscriptions' model =
  Sub.none


type Page
  = Home
  |Locay


type Msg
  = FetchUser
  | FetchUserComplete Account.User
  | FetchUserFailed Http.Error
  | Change String
  | GoToUpdateLocation
  | LocationChanged (List Location)
  | UpdateLocationMsg EditUserInformation.Msg


fetchUser : String -> Task Http.Error Account.User
fetchUser userId =
  Http.get Account.decoder ("/api/" ++ userId)


updateUserLocation: List Location -> Account.User -> Account.User
updateUserLocation updatedLocation user =
  {user | locations = updatedLocation}


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change input ->
      ({ model | userIdInput = ( Debug.log "input" input) } , Cmd.none )
    FetchUser ->
      (model,
       Task.perform FetchUserFailed FetchUserComplete ( fetchUser (Debug.log "fetching" model.userIdInput)))
    FetchUserComplete userInfo ->
      let
        inputLocation =
          EditUserInformation.init userInfo
      in

      ({ model | user = userInfo} , Cmd.none )
    FetchUserFailed err ->
      ({ model | fetchError = Just ( Debug.log "failedFetch" err) } , Cmd.none )

    GoToUpdateLocation ->
      let
        inputLocation =
          EditUserInformation.init model.user
      in
        ({model | page = Locay, inputLocation = inputLocation }, Cmd.map UpdateLocationMsg Cmd.none)

    UpdateLocationMsg msg ->
      let
        (update, updateMsg) =
          EditUserInformation.update msg model.user model.inputLocation
      in
        ({model | inputLocation = update}, Cmd.map UpdateLocationMsg updateMsg)
    LocationChanged updatedLocation->
      {model | page = Home, user = updateUserLocation updatedLocation model.user} ![]



viewHome : Model -> Html Msg
viewHome model =
  let errorUser =
    case model.fetchError of
      Nothing ->
        []
      Just err ->
        [text ("  " ++ toString err)]
  in
    body[][
      div [ id "header"]
        [ h2 []
          [text model.header]
        ]
      , div [ id "fetchUser"]
        ([ input [ placeholder "User ID", onInput Change ] []
        , button [ onClick FetchUser ] [text "Go to User"]
        ]
          ++ errorUser)
      , div [ id "username"]
        [ h1 []
          [text "Username"]
        , p []
          [text model.user.name]
        ]
{-
      , div []
        [ h1 []
          [text "User ID"]
        , p[]
          [text (toString model.user.id)]
        ]
-}
      , div [ id "accounts"]
        [ h1 [][text "Accounts"]
        , p []
          [text "we aren't doing this right now"]
        ]

      , div [ id "email"]
        [ h1 []
          [text "Your email"]
        , p []
          [text (toString model.user.emails)]
        ]
      , div [ id "location"]
        [ h1 []
          [text "Your location(s)"]
        , ul []
          (List.map (li []
            << EditUserInformation.htmlToList
            << text
            << Location.toString)
            model.user.locations)
        ]
      , div [ id "updateLocay" ]
        [
          button [ onClick GoToUpdateLocation ]
          [text "update"]
        ]
      ]

view :  Model -> Html Msg
view model =
  case model.page of
    Home ->
      viewHome model
    Locay ->
      Html.App.map UpdateLocationMsg (EditUserInformation.view model.inputLocation)
