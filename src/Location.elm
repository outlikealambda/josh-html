module Location exposing (..)
import Json.Decode as Json exposing ((:=))
import Json.Encode as Encode


type alias Location =
  { name: String
  , id: Int
  , country: String
  , city: String
  , postal: String
  }


decoder : Json.Decoder Location
decoder =
  Json.object5
    Location
    ("name" := Json.string)
    ("id" := Json.int)
    ("country" := Json.string)
    ("city" := Json.string)
    ("postal" := Json.string)

encoder : Location -> Encode.Value
encoder {name, id, country, city, postal} =
  Encode.object
    [ ("name", Encode.string name)
    --, ("id", Encode.int id)
    , ("country", Encode.string country)
    , ("city", Encode.string city)
    , ("postal", Encode.string postal)
    ]