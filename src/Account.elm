module Account exposing (..)
import Json.Decode as Json exposing ((:=))


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
