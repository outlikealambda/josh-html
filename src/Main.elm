module Main exposing (..)
import Html.App as Html
import List
import Html.Attributes exposing (id, list, href)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a)


main =
  Html.beginnerProgram {model = init, view = view, update = update }

update msg model = model

init : Model
init =
  { header = "User Info"
  , name = "Mike Jefferson"
  , accounts =
      [{ description = "Facebook"
      , url = "Facebook.com"
      }
      ,{ description = "Twitter"
      , url = "Twitter.com"
      }
      ,{ description = "GitHub"
      , url = "GitHub.com"
      }]
  , email = "ex@ex.com"
  , location = "Honolulu, Hawaii"
  }

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

viewAccount : Account -> Html Never
viewAccount accountInfo =
  li []
    [text accountInfo.url]

view :  Model -> Html Never
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
