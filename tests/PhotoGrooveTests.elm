module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Json.Encode as Encode
import Json.Decode as Decode exposing (decodeString, decodeValue)
import Fuzz exposing (Fuzzer, int, list, string)
import PhotoGroove
import Test exposing (..)

-- Naive approach
-- decoderTest : Test
-- decoderTest =
--     test "title defaults to (untitled)" 
--       <| \_ -> 
--         """{"url": "fruits.com", "size": 5}"""
--         |> decodeString PhotoGroove.photoDecoder
--         |> Expect.equal
--           (Ok { url = "fruits.com", size = 5, title = "(untitled)" })

-- Generative Testing
decoderFuzzTest : Test
decoderFuzzTest =
  fuzz2 string int "title defaults to (untitled)" <|
      \url size ->
        [ ("url", Encode.string url )
        , ("size", Encode.int size )
        ]
        |> Encode.object
        |> decodeValue PhotoGroove.photoDecoder
        |> Result.map .title
        |> Expect.equal (Ok "(untitled)")
