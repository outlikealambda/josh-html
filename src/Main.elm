module Main exposing (..)
import Html.App as Html
import Html.Attributes exposing (id)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html)


main =
  Html.beginnerProgram {model = init, view = view, update = update }

update msg model = model

init : Model
init =
  { header = "User Info"
  , name = "Mike Jefferson"
  , accounts = "Your other accounts"
  , email = "ex@ex.com"
  , location = "Honolulu, Hawaii"
  }

type alias Model =
  { header : String
  , name : String
  , accounts : String
  , email : String
  , location : String
  }

view : Model -> Html Never
view model =
  body []
    [ div [ id "header"]
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
      [ h1 []
        [text model.accounts]
      , ul []
        [ li []
          [text "Facebook"]
        , li []
          [text "Twitter"]
        , li []
          [text "GitHub"]
        ]
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
