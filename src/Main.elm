module Main exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
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
  , user =
      { name = ""
      , id = 0
      , emails = []
      , location =
        { name = ""
        , id = 0
        , country = ""
        , city = ""
        , postal = 0
        }
      }
  , inputField = ""
  }, Cmd.none)

subscriptions' : Model -> Sub Msg
subscriptions' model =
  Sub.none

type alias Model =
  { fetchError : Maybe Http.Error
  , inputField : String
  , header : String
  , user : Account
  }

type alias Account =
  { name: String
  , id: Int
  --, trustee: List Trustee
  , emails: List String
  , location : Location
  }

type alias Location =
  { name: String
  , id: Int
  , country: String
  , city: String
  , postal: Int
  }

type Msg
  = FetchUser
  | FetchUserComplete Account
  | FetchUserFailed Http.Error
  | Change String


modelDecoder : String -> Json.Decoder Model
modelDecoder inputField =
  Json.object2
    (Model Nothing inputField)
    ("header" := Json.string)
    ("user" := accountDecoder)

accountDecoder : Json.Decoder Account
accountDecoder =
  Json.object4
    Account
    ("name" := Json.string)
    ("id" := Json.int)
    ("emails" := Json.list Json.string)
    ("location" := locationDecoder)

locationDecoder : Json.Decoder Location
locationDecoder =
  Json.object5
    Location
    ("name" := Json.string)
    ("id" := Json.int)
    ("country" := Json.string)
    ("city" := Json.string)
    ("postal" := Json.int)


fetchUser : String -> Task Http.Error Account
fetchUser userId =
  Http.get accountDecoder ("/api/" ++ userId)


{-
viewAccount : Account -> Html Msg
viewAccount accountInfo =
  li []
    [text accountInfo.url]
-}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change input ->
       ({ model | inputField = ( Debug.log "input" input) } , Cmd.none )
    FetchUser ->
       (model, Task.perform FetchUserFailed FetchUserComplete ( fetchUser (Debug.log "fetching" model.inputField)))
    FetchUserComplete userInfo ->
       ({ model | user = userInfo } , Cmd.none )
    FetchUserFailed err ->
       ({ model | fetchError = Just ( Debug.log "failedFetch" err) } , Cmd.none )

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
        ([ input [ placeholder "User ID", onInput Change ] []
        , button [ onClick FetchUser ] [text "Go to User"]
        ]
          ++ errorUser)
      , div [ id "username"]
        [ h1 []
          [text "username"]
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
      ]
