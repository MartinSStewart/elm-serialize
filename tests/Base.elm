module Base exposing (roundtrips, suite)

import Basics.Extra
import Bytes exposing (Bytes)
import Bytes.Encode
import Codec.Serialize as Codec exposing (Codec)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Set
import Test exposing (Test, describe, fuzz, test)
import Toop exposing (T1(..), T6(..))


suite : Test
suite =
    describe "Testing roundtrips"
        [ describe "Basic" basicTests
        , describe "Containers" containersTests
        , describe "Object" objectTests
        , describe "Custom" customTests
        , describe "map" bimapTests
        , describe "andThen" andThenTests
        , describe "errorTests" errorTests
        , describe "lazy" lazyTests
        , describe "maybe" maybeTests
        , describe "constant"
            [ test "roundtrips"
                (\_ ->
                    Codec.constant 632
                        |> (\d -> Codec.fromBytes d (Bytes.Encode.sequence [] |> Bytes.Encode.encode))
                        |> Expect.equal (Ok 632)
                )
            ]
        , describe "errorToString" errorToStringTest
        , describe "enum" enumTest
        ]


roundtrips : Fuzzer a -> Codec a -> Test
roundtrips fuzzer codec =
    fuzz fuzzer "is a roundtrip" <|
        \value ->
            Expect.all
                [ Codec.toBytes codec >> Codec.fromBytes codec >> Expect.equal (Ok value)
                , Codec.toString codec >> Codec.fromString codec >> Expect.equal (Ok value)
                ]
                value


basicTests : List Test
basicTests =
    [ describe "Codec.string"
        [ roundtrips Fuzz.string Codec.string
        ]
    , describe "Codec.int"
        [ roundtrips maxRangeIntFuzz Codec.int
        ]
    , describe "Codec.float64"
        [ roundtrips Fuzz.float Codec.float
        ]
    , describe "Codec.bool"
        [ roundtrips Fuzz.bool Codec.bool
        ]
    , describe "Codec.char"
        [ roundtrips charFuzz Codec.char
        ]
    , describe "Codec.bytes"
        [ roundtrips fuzzBytes Codec.bytes
        ]
    ]


fuzzBytes : Fuzzer Bytes
fuzzBytes =
    Fuzz.list maxRangeIntFuzz |> Fuzz.map (List.map (Bytes.Encode.unsignedInt32 Bytes.LE) >> Bytes.Encode.sequence >> Bytes.Encode.encode)


containersTests : List Test
containersTests =
    [ describe "Codec.array"
        [ roundtrips (Fuzz.array maxRangeIntFuzz) (Codec.array Codec.int)
        ]
    , describe "Codec.list"
        [ roundtrips (Fuzz.list maxRangeIntFuzz) (Codec.list Codec.int)
        ]
    , describe "Codec.dict"
        [ roundtrips
            (Fuzz.map2 Tuple.pair Fuzz.string maxRangeIntFuzz
                |> Fuzz.list
                |> Fuzz.map Dict.fromList
            )
            (Codec.dict Codec.string Codec.int)
        ]
    , describe "Codec.set"
        [ roundtrips
            (Fuzz.list maxRangeIntFuzz |> Fuzz.map Set.fromList)
            (Codec.set Codec.int)
        ]
    , describe "Codec.tuple"
        [ roundtrips
            (Fuzz.tuple ( maxRangeIntFuzz, maxRangeIntFuzz ))
            (Codec.tuple Codec.int Codec.int)
        ]
    ]


maxRangeIntFuzz =
    Fuzz.intRange Basics.Extra.minSafeInteger Basics.Extra.maxSafeInteger


charFuzz =
    [ '😀', 'ß', Char.toUpper 'ß', 'a', '吧', '吗', '\t' ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


objectTests : List Test
objectTests =
    [ describe "with 0 fields"
        [ roundtrips (Fuzz.constant {})
            (Codec.record {}
                |> Codec.finishRecord
            )
        ]
    , describe "with 1 field"
        [ roundtrips (Fuzz.map (\i -> { fname = i }) maxRangeIntFuzz)
            (Codec.record (\i -> { fname = i })
                |> Codec.field .fname Codec.int
                |> Codec.finishRecord
            )
        ]
    , describe "with 2 fields"
        [ roundtrips
            (Fuzz.map2
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                maxRangeIntFuzz
                maxRangeIntFuzz
            )
            (Codec.record
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                |> Codec.field .a Codec.int
                |> Codec.field .b Codec.int
                |> Codec.finishRecord
            )
        ]
    ]


customTests : List Test
customTests =
    [ describe "with 1 ctor, 0 args"
        [ roundtrips (Fuzz.constant ())
            (Codec.customType
                (\f v ->
                    case v of
                        () ->
                            f
                )
                |> Codec.variant0 ()
                |> Codec.finishCustomType
            )
        ]
    , describe "with 1 ctor, 1 arg"
        [ roundtrips (Fuzz.map T1 maxRangeIntFuzz)
            (Codec.customType
                (\f v ->
                    case v of
                        T1 a ->
                            f a
                )
                |> Codec.variant1 T1 Codec.int
                |> Codec.finishCustomType
            )
        ]
    , describe "with 1 ctor, 6 arg"
        [ roundtrips (Fuzz.map5 (T6 0) maxRangeIntFuzz maxRangeIntFuzz maxRangeIntFuzz maxRangeIntFuzz maxRangeIntFuzz)
            (Codec.customType
                (\function v ->
                    case v of
                        T6 a b c d e f ->
                            function a b c d e f
                )
                |> Codec.variant6 T6 Codec.int Codec.int Codec.int Codec.int Codec.int Codec.int
                |> Codec.finishCustomType
            )
        ]
    , describe "with 2 ctors, 0,1 args" <|
        let
            match fnothing fjust value =
                case value of
                    Nothing ->
                        fnothing

                    Just v ->
                        fjust v

            codec =
                Codec.customType match
                    |> Codec.variant0 Nothing
                    |> Codec.variant1 Just Codec.int
                    |> Codec.finishCustomType

            fuzzers =
                [ ( "1st ctor", Fuzz.constant Nothing )
                , ( "2nd ctor", Fuzz.map Just maxRangeIntFuzz )
                ]
        in
        fuzzers
            |> List.map
                (\( name, fuzz ) ->
                    describe name
                        [ roundtrips fuzz codec ]
                )
    ]


bimapTests : List Test
bimapTests =
    [ roundtrips Fuzz.float <|
        Codec.map
            (\x -> x * 2)
            (\x -> x / 2)
            Codec.float
    ]


{-| Volume must be between 0 and 1.
-}
volumeCodec =
    Codec.float
        |> Codec.andThen
            (\volume ->
                if volume <= 1 && volume >= 0 then
                    Ok volume

                else
                    Err "Volume is outside of valid range."
            )
            (\volume -> volume)


andThenTests : List Test
andThenTests =
    [ roundtrips (Fuzz.floatRange 0 1) <| volumeCodec
    , test "andThen fails on invalid binary data." <|
        \_ ->
            5
                |> Codec.toBytes volumeCodec
                |> Codec.fromBytes volumeCodec
                |> Expect.equal (Codec.AndThenCodecError "Volume is outside of valid range." |> Err)
    ]


type alias Record =
    { a : Int
    , b : Float
    , c : String
    , d : String
    }


errorTests : List Test
errorTests =
    [ test "variant produces correct error message." <|
        \_ ->
            let
                codec =
                    Codec.customType
                        (\encodeNothing encodeJust value ->
                            case value of
                                Nothing ->
                                    encodeNothing

                                Just v ->
                                    encodeJust v
                        )
                        |> Codec.variant0 Nothing
                        |> Codec.variant1 Just Codec.int
                        |> Codec.finishCustomType

                codecBad =
                    Codec.customType
                        (\encodeNothing _ encodeJust value ->
                            case value of
                                Nothing ->
                                    encodeNothing

                                Just v ->
                                    encodeJust v
                        )
                        |> Codec.variant0 Nothing
                        |> Codec.variant0 Nothing
                        |> Codec.variant1 Just Codec.int
                        |> Codec.finishCustomType
            in
            Codec.toBytes codecBad (Just 0) |> Codec.fromBytes codec |> Expect.equal (Err Codec.NoVariantMatches)
    , test "list produces correct error message." <|
        \_ ->
            let
                codec =
                    Codec.list volumeCodec
            in
            Codec.toBytes codec [ 0, 3, 0, 4, 0, 0 ]
                |> Codec.fromBytes codec
                |> Expect.equal
                    (Codec.ListCodecError
                        { listIndex = 1
                        , error = Codec.AndThenCodecError "Volume is outside of valid range."
                        }
                        |> Err
                    )
    , test "Record produces correct error message." <|
        \_ ->
            let
                codec =
                    Codec.record Record
                        |> Codec.field .a Codec.int
                        |> Codec.field .b volumeCodec
                        |> Codec.field .c Codec.string
                        |> Codec.field .d Codec.string
                        |> Codec.finishRecord
            in
            Codec.toBytes codec { a = 0, b = -1, c = "", d = "" }
                |> Codec.fromBytes codec
                |> Expect.equal
                    (Codec.RecordCodecError
                        { fieldIndex = 1
                        , error = Codec.AndThenCodecError "Volume is outside of valid range."
                        }
                        |> Err
                    )
    ]


type Peano
    = Peano (Maybe Peano)


{-| This is the same example used in Codec.recursive but adapted for lazy.
-}
peanoCodec : Codec Peano
peanoCodec =
    Codec.maybe (Codec.lazy (\() -> peanoCodec)) |> Codec.map Peano (\(Peano a) -> a)


lazyTests : List Test
lazyTests =
    [ roundtrips peanoFuzz peanoCodec
    ]


peanoFuzz : Fuzzer Peano
peanoFuzz =
    Fuzz.intRange 0 10 |> Fuzz.map (intToPeano Nothing)


intToPeano : Maybe Peano -> Int -> Peano
intToPeano peano value =
    if value <= 0 then
        Peano Nothing

    else
        intToPeano peano (value - 1) |> Just |> Peano


maybeTests : List Test
maybeTests =
    [ describe "single"
        [ roundtrips
            (Fuzz.oneOf
                [ Fuzz.constant Nothing
                , Fuzz.map Just maxRangeIntFuzz
                ]
            )
          <|
            Codec.maybe Codec.int
        ]
    ]


errorToStringTest : List Test
errorToStringTest =
    [ test "customTypeError" <|
        \_ ->
            let
                expected =
                    "An error occured in a custom type codec, in the 1st variant called. The line looks something like this (x marks the position of the parameter that failed to decode):\n"
                        ++ "|> variant2 _ _ x\n\nwith this error message:\n    Something broke.\n\n\n"
            in
            { variantIndex = 0, variantSize = 2, variantConstructorIndex = 1, error = Codec.AndThenCodecError "Something broke." }
                |> Codec.CustomTypeCodecError
                |> Codec.errorToString
                |> Expect.equal expected
    ]


type DaysOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


daysOfWeekCodec =
    Codec.enum Monday [ Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]


badDaysOfWeekCodec =
    Codec.enum Monday []


daysOfWeekFuzz =
    [ Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


enumTest : List Test
enumTest =
    [ roundtrips daysOfWeekFuzz daysOfWeekCodec
    , test "Default to first item when encoding if item doesn't exist." <|
        \_ ->
            Codec.toBytes badDaysOfWeekCodec Tuesday |> Codec.fromBytes badDaysOfWeekCodec |> Expect.equal (Ok Monday)
    , test "Error if enum index is greater than number of values in enum." <|
        \_ ->
            Codec.toBytes daysOfWeekCodec Tuesday |> Codec.fromBytes badDaysOfWeekCodec |> Expect.equal (Err Codec.EnumCodecValueNotFound)
    ]
