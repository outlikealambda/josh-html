module Account exposing (..)
import Json.Decode as Json exposing ((:=))
import Location
import Json.Encode as Encode


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
    (Json.oneOf ["location" := Location.decoder, Json.succeed
      Location.empty])

{-encoder : User -> Encode.Value
encoder {name,id,emails,location} =
  [ ("name", Encode.string name)
  , ("id", Encode.int id)
  , ("emails", Encode.list Encode.string emails)
  , ("location", Location.encoder location)
  ]-}
