module FileSizeTests exposing (..)

import AstCodec
import Bytes
import Bytes.Encode
import Elm.Parser
import Elm.Processing
import Expect
import Json.Encode
import Serialize
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "File size test"
        [ Test.only <|
            test "encodeToBytes file size" <|
                \_ ->
                    case Elm.Parser.parse code of
                        Ok parsed ->
                            let
                                _ =
                                    Debug.log "parsed" ()
                            in
                            Elm.Processing.process Elm.Processing.init parsed
                                |> Serialize.encodeToJson AstCodec.file
                                |> Json.Encode.encode 0
                                |> Debug.log "json"
                                |> Bytes.Encode.getStringWidth
                                --|> Bytes.width
                                |> Debug.log "bytes"
                                |> Expect.lessThan 100

                        Err error ->
                            Expect.fail ("Failed to parse: " ++ Debug.toString error)
        ]


code : String
code =
    """module Serialize exposing (..)
"""
