module Main exposing (..)

import AstCodec
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import Elm.Parser
import Elm.Processing
import Serialize as S


suite : Benchmark
suite =
    describe "Array"
        [ describe "Encoding"
            [ benchmark "bytes" <|
                \_ -> code |> Result.map (S.encodeToBytes AstCodec.file)
            , benchmark "json" <|
                \_ -> code |> Result.map (S.encodeToJson AstCodec.file)
            , benchmark "string" <|
                \_ -> code |> Result.map (S.encodeToString AstCodec.file)
            ]
        ]


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
