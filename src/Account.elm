module Account exposing (..)
import Json.Decode as Json exposing ((:=))
import Location exposing (Location)

type alias User =
  { name: String
  , id: Int
  , emails: List Email
  , locations : List Location
  }


type alias Email = String

addLocation : User -> Location -> List Location
addLocation user location =
  (location :: user.locations)

listToUser : List Location -> User -> User
listToUser locations user =
  {user | locations = locations}

decoder : Json.Decoder User
decoder =
  Json.object4
    User
    ("name" := Json.string)
    ("id" := Json.int)
    ("emails" := Json.list Json.string)
    ("location" := Json.list Location.decoder)
