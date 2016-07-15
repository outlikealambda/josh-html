module Account exposing (..)
import Json.Decode as Json exposing ((:=))
import Location exposing (Location)

type alias User =
  { name: String
  , id: Int
  --, trustee: List Trustee
  , emails: List Email
  , locations : List Location
  }

type alias Email = String

addLocation : User -> Location -> List Location
addLocation user location =
  (location :: user.locations)

listToUser : List Location -> User -> User
listToUser locations user =
  user

decoder : Json.Decoder User
decoder =
  Json.object4
    User
    ("name" := Json.string)
    ("id" := Json.int)
    ("emails" := Json.list Json.string)
    ("location" := Json.list Location.decoder)
{-
    (Json.oneOf ["location" := Json.list Location.decoder, Json.succeed
      Location.empty])
      -}

{-encoder : User -> Encode.Value
encoder {name,id,emails,location} =
  [ ("name", Encode.string name)
  , ("id", Encode.int id)
  , ("emails", Encode.list Encode.string emails)
  , ("location", Location.encoder location)
  ]-}
