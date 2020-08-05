module Main exposing (..)

{-| These benchmarks take a while to run so don't worry if the browser doesn't show anything for several minutes.
-}

import AstCodec
import AstCodecV1
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import Elm.Parser
import Elm.Processing
import Serialize as S
import SerializeV1


suite : Benchmark
suite =
    describe "Benchmarks"
        [ describe "Encoding"
            [ benchmark "bytes" <|
                \_ -> code |> Result.map (S.encodeToBytes AstCodec.file)
            , benchmark "bytes v1" <|
                \_ -> code |> Result.map (SerializeV1.encodeToBytes AstCodecV1.file)
            , benchmark "json" <|
                \_ -> code |> Result.map (S.encodeToJson AstCodec.file)
            , benchmark "string" <|
                \_ -> code |> Result.map (S.encodeToString AstCodec.file)
            , benchmark "string v1" <|
                \_ -> code |> Result.map (SerializeV1.encodeToString AstCodecV1.file)
            ]
        , decodingBenchmarks
        ]


decodingBenchmarks : Benchmark
decodingBenchmarks =
    code
        |> Result.map
            (\file ->
                let
                    bytesData =
                        S.encodeToBytes AstCodec.file file

                    jsonData =
                        S.encodeToJson AstCodec.file file

                    stringData =
                        S.encodeToString AstCodec.file file
                in
                [ benchmark "bytes" <|
                    \_ -> S.decodeFromBytes AstCodec.file bytesData
                , benchmark "bytes v1" <|
                    \_ -> SerializeV1.decodeFromBytes AstCodecV1.file bytesData
                , benchmark "json" <|
                    \_ -> S.decodeFromJson AstCodec.file jsonData
                , benchmark "string" <|
                    \_ -> S.decodeFromString AstCodec.file stringData
                , benchmark "string v1" <|
                    \_ -> SerializeV1.decodeFromString AstCodecV1.file stringData
                ]
            )
        |> Result.toMaybe
        |> Maybe.withDefault []
        |> describe "Decoding"


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


code =
    Result.map (Elm.Processing.process Elm.Processing.init) <|
        Elm.Parser.parse <|
            """module Serialize exposing(..)


lazy : (() -> Codec e a) -> Codec e a
lazy f =
    build
        (\\value -> getEncoder (f ()) value)
        (BD.succeed () |> BD.andThen (\\() -> getDecoder (f ())))

"""
