module Main exposing (..)
import Html.App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (id, list, href, placeholder)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
import Http
import Task exposing (Task)
import EditUserInformation
import Account



main : Program Never
main =
  Html.App.program {init = init, view = view, update = update, subscriptions = subscriptions'}

type alias Model =
  { fetchError : Maybe Http.Error
  , userIdInput : String
  , header : String
  , user : Account.User
  , page : Page
  , inputLocation : EditUserInformation.Model
  }

init : (Model, Cmd Msg)
init =
  let
     user =
        { name = ""
        , id = -1
        , emails = []
        , location =
          { name = ""
          , id = -1
          , country = ""
          , city = ""
          , postal = ""
          }
        }
  in
    ({page = Home
    , fetchError = Nothing
    , header = "User Information"
    , userIdInput = ""
    , user = user
    , inputLocation = EditUserInformation.init user
    }, Cmd.none)

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
  | UpdateLocationMsg EditUserInformation.Msg
  | FullLocationUpdate EditUserInformation.Msg


{-
--I don't even know if this is ever used
modelDecoder : String -> Json.Decoder Model
modelDecoder inputField =
  Json.object2
    (Model Nothing inputField)
    ("header" := Json.string)
    ("user" := accountDecoder)
    -}



fetchUser : String -> Task Http.Error Account.User
fetchUser userId =
  Http.get Account.decoder ("/api/" ++ userId)


{-
--I don't even know if this is ever used
viewAccount : Account -> Html Msg
viewAccount accountInfo =
  li []
    [text accountInfo.url]
-}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change input ->
      ({ model | userIdInput = ( Debug.log "input" input) } , Cmd.none )
    FetchUser ->
      (model, Task.perform FetchUserFailed FetchUserComplete ( fetchUser (Debug.log "fetching" model.userIdInput)))
    FetchUserComplete userInfo ->
      ({ model | user = userInfo, inputLocation = EditUserInformation.init userInfo } , Cmd.none )
    FetchUserFailed err ->
      ({ model | fetchError = Just ( Debug.log "failedFetch" err) } , Cmd.none )
    GoToUpdateLocation ->
      ({model | page = Locay, inputLocation = EditUserInformation.init model.user}, Cmd.none)
    UpdateLocationMsg msg ->
      ({model | inputLocation = EditUserInformation.update msg model.inputLocation}, Cmd.none)
    FullLocationUpdate msg ->
      ({model | page = Home }, Cmd.none)

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
      , div []
        [ h1 []
          [text "User ID"]
        , p[]
          [text (toString model.user.id)]
        ]

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
          [text "Your location"]
        , p []
          [text (toString model.user.location)]
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
