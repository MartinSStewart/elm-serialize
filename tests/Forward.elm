module Forward exposing (suite)

import Base
import Codec.Serialize as Codec exposing (Codec)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Testing forward and backward compatability"
        [ describe "Any to constant" anyToConstant
        ]


compatible : Fuzzer a -> (a -> b) -> Codec a -> Codec b -> Test
compatible fuzzer map oldCodec newCodec =
    fuzz fuzzer "compatible" <|
        \value ->
            value
                |> Codec.toBytes oldCodec
                |> Codec.fromBytes newCodec
                |> Expect.equal (Ok <| map value)


forward : Fuzzer old -> (old -> new) -> Codec old -> Codec new -> Test
forward fuzzer map oldCodec newCodec =
    describe "forward"
        [ describe "old"
            [ Base.roundtrips fuzzer oldCodec
            ]
        , describe "new"
            [ Base.roundtrips (Fuzz.map map fuzzer) newCodec
            ]
        , describe "old value with new codec"
            [ compatible fuzzer map oldCodec newCodec
            ]
        ]


both :
    Fuzzer old
    -> (old -> new)
    -> Codec old
    -> Fuzzer new
    -> (new -> old)
    -> Codec new
    -> List Test
both oldFuzzer oldToNew oldCodec newFuzzer newToOld newCodec =
    [ describe "old"
        [ Base.roundtrips oldFuzzer oldCodec
        ]
    , describe "new"
        [ Base.roundtrips newFuzzer newCodec
        ]
    , describe "old value with new codec"
        [ compatible oldFuzzer oldToNew oldCodec newCodec
        ]
    , describe "new value with old codec"
        [ compatible newFuzzer newToOld newCodec oldCodec
        ]
    ]


anyToConstant : List Test
anyToConstant =
    [ forward Fuzz.string (always 3) Codec.string (Codec.constant 3)
    ]
