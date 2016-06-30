module Account exposing (..)
import Json.Decode as Json exposing ((:=))
import Location


type alias User =
  { name: String
  , id: Int
  --, trustee: List Trustee
  , emails: List String
  , location : Location.Location
  }


decoder : Json.Decoder User
decoder =
  Json.object4
    User
    ("name" := Json.string)
    ("id" := Json.int)
    ("emails" := Json.list Json.string)
    ("location" := Location.decoder)
