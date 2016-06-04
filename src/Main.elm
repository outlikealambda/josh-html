module Main exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import List
import Html.Attributes exposing (id, list, href)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (Task)


main : Program Never
main =
  Html.program {init = init, view = view, update = update, subscriptions = subscriptions'}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchUser -> (model, Task.perform FetchUserFailed FetchUserComplete (fetchUser "mike"))
    FetchUserComplete user -> (user, Cmd.none)
    FetchUserFailed _ -> (model, Cmd.none)

init : (Model, Cmd Msg)
init =
  ({header = ""
  ,name = ""
  ,accounts = []
  ,email = ""
  ,location = ""
  }, Cmd.none)

subscriptions' : Model -> Sub Msg
subscriptions' model =
  Sub.none

type alias Model =
  { header : String
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

modelDecoder : Json.Decoder Model
modelDecoder =
  Json.object5
    Model
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
  Http.get modelDecoder ("/" ++ name ++ ".json")

viewAccount : Account -> Html Msg
viewAccount accountInfo =
  li []
    [text accountInfo.url]

view :  Model -> Html Msg
view model =
  body[][
    div [ id "fetch user"]
    [ button [ onClick FetchUser ] [text "Click me!"]
    ]
  , div [ id "header"]
    [ h2 []
      [text model.header]
    ]
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
