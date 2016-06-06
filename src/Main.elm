module Main exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import List
import Html.Attributes exposing (id, list, href, placeholder)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (Task)


main : Program Never
main =
  Html.program {init = init, view = view, update = update, subscriptions = subscriptions'}


init : (Model, Cmd Msg)
init =
  ({ fetchError = Nothing
  , header = "User Information"
  ,name = ""
  ,accounts = []
  ,email = ""
  ,location = ""
  ,inputField = ""
  }, Cmd.none)

subscriptions' : Model -> Sub Msg
subscriptions' model =
  Sub.none

type alias Model =
  { fetchError : Maybe Http.Error
  , inputField : String
  , header : String
  , name : String
  , accounts : List Account
  , email : String
  , location : String
  }

type alias Account =
  { description : String
  , url : String
  }

type Msg
  = FetchUser
  | FetchUserComplete Model
  | FetchUserFailed Http.Error
  | Change String


modelDecoder : String -> Json.Decoder Model
modelDecoder inputField =
  Json.object5
    (Model Nothing inputField)
    ("header" := Json.string)
    ("name" := Json.string)
    ("accounts" := Json.list accountDecoder)
    ("email" := Json.string)
    ("location" := Json.string)

accountDecoder : Json.Decoder Account
accountDecoder =
  Json.object2
    Account
    ("description" := Json.string)
    ("url" := Json.string)


fetchUser : String -> Task Http.Error Model
fetchUser name =
  Http.get (modelDecoder name) ("/api/users/" ++ name)

viewAccount : Account -> Html Msg
viewAccount accountInfo =
  li []
    [text accountInfo.url]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change input -> ({ model | inputField = ( Debug.log "input" input) } , Cmd.none )
    FetchUser -> (model, Task.perform FetchUserFailed FetchUserComplete ( fetchUser (Debug.log "fetching" model.inputField)))
    FetchUserComplete user -> (user, Cmd.none)
    FetchUserFailed err ->  ({ model | fetchError = Just ( Debug.log "failedFetch" err) } , Cmd.none )

view :  Model -> Html Msg
view model =
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
        ([ input [ placeholder "username", onInput Change ] []
        , button [ onClick FetchUser ] [text "Go to User"]
        ]
          ++ errorUser)
      , div [ id "username"]
        [ h1 []
          [text "You are"]
        , p []
          [text model.name]
        ]

      , div [ id "accounts"]
        [ h1 [][text "Accounts"]
        , ul [] (List.map viewAccount model.accounts)
        ]

      , div [ id "email"]
        [ h1 []
          [text "Your email"]
        , p []
          [text model.email]
        ]
      , div [ id "location"]
        [ h1 []
          [text "Your location"]
        , p []
          [text model.location]
        ]
      ]
