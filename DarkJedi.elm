module DarkJedi exposing (..)

import Json.Encode
import Json.Decode
-- elm-package install --yes NoRedInk/elm-decode-pipeline
import Json.Decode.Pipeline

type alias DarkJedi =
  { id : Int
  , name : String
  , homeworld : Homeworld
  , master : Master
  , apprentice : DarkJediApprentice
  }

type alias Homeworld =
  { id : Int
  , name : String
  }

type alias Master =
  { url : String
  , id : Int
  }

type alias DarkJediApprentice =
  { url : String
  , id : Int
  }

decodeDarkJedi : Json.Decode.Decoder DarkJedi
decodeDarkJedi =
  Json.Decode.Pipeline.decode DarkJedi
    |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
    |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
    |> Json.Decode.Pipeline.required "homeworld" (decodeHomeworld)
    |> Json.Decode.Pipeline.required "master" (decodeDarkJediMaster)
    |> Json.Decode.Pipeline.required "apprentice" (decodeDarkJediApprentice)

decodeHomeworld : Json.Decode.Decoder Homeworld
decodeHomeworld =
  Json.Decode.Pipeline.decode Homeworld
    |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
    |> Json.Decode.Pipeline.required "name" (Json.Decode.string)

decodeDarkJediMaster : Json.Decode.Decoder Master
decodeDarkJediMaster =
  Json.Decode.Pipeline.decode Master
    |> Json.Decode.Pipeline.required "url" (Json.Decode.string)
    |> Json.Decode.Pipeline.required "id" (Json.Decode.int)

decodeDarkJediApprentice : Json.Decode.Decoder DarkJediApprentice
decodeDarkJediApprentice =
  Json.Decode.Pipeline.decode DarkJediApprentice
    |> Json.Decode.Pipeline.required "url" (Json.Decode.string)
    |> Json.Decode.Pipeline.required "id" (Json.Decode.int)

encodeDarkJedi : DarkJedi -> Json.Encode.Value
encodeDarkJedi record =
  Json.Encode.object
    [ ("id",  Json.Encode.int <| record.id)
    , ("name",  Json.Encode.string <| record.name)
    , ("homeworld",  encodeHomeworld <| record.homeworld)
    , ("master",  encodeDarkJediMaster <| record.master)
    , ("apprentice",  encodeDarkJediApprentice <| record.apprentice)
    ]

encodeHomeworld : Homeworld -> Json.Encode.Value
encodeHomeworld record =
  Json.Encode.object
    [ ("id",  Json.Encode.int <| record.id)
    , ("name",  Json.Encode.string <| record.name)
    ]

encodeDarkJediMaster : Master -> Json.Encode.Value
encodeDarkJediMaster record =
  Json.Encode.object
    [ ("url",  Json.Encode.string <| record.url)
    , ("id",  Json.Encode.int <| record.id)
    ]

encodeDarkJediApprentice : DarkJediApprentice -> Json.Encode.Value
encodeDarkJediApprentice record =
  Json.Encode.object
    [ ("url",  Json.Encode.string <| record.url)
    , ("id",  Json.Encode.int <| record.id)
    ]