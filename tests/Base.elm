module Base exposing (roundtrips, suite)

import Basics.Extra
import Bytes exposing (Bytes)
import Bytes.Encode
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Serialize as S exposing (Codec)
import SerializeV1
import Set
import Test exposing (Test, describe, fuzz, test)
import Toop exposing (T1(..), T2(..), T3(..), T4(..), T5(..), T6(..), T7(..), T8(..))
import Url


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

        --, describe "errorToString" errorToStringTest
        , describe "enum" enumTest
        , fuzz fuzzBytes "toString is url safe" <|
            \bytes ->
                let
                    expected =
                        S.encodeToString S.bytes bytes
                in
                expected |> Url.percentEncode |> Expect.equal expected
        , describe "Serializer version" serializerVersionTests
        , Test.fuzz Fuzz.float "Json round trip float" <|
            \value -> String.fromFloat value |> String.toFloat |> Expect.equal (Just value)
        ]


roundtrips : Fuzzer a -> Codec e a -> SerializeV1.Codec e a -> Test
roundtrips fuzzer codec codecV1 =
    fuzz fuzzer "is a roundtrip" (roundtripHelper codec codecV1)


roundtripHelper : Codec e a -> SerializeV1.Codec e1 a -> a -> Expect.Expectation
roundtripHelper codec codecV1 value =
    Expect.all
        [ S.encodeToBytes codec >> S.decodeFromBytes codec >> Expect.equal (Ok value)
        , S.encodeToString codec >> S.decodeFromString codec >> Expect.equal (Ok value)
        , S.encodeToJson codec >> S.decodeFromJson codec >> Expect.equal (Ok value)
        , S.encodeToJson codec >> Json.Decode.decodeValue (S.getJsonDecoder (always "") codec) >> Expect.equal (Ok value)
        , SerializeV1.encodeToBytes codecV1 >> S.decodeFromBytes codec >> Expect.equal (Ok value)
        , SerializeV1.encodeToString codecV1 >> S.decodeFromString codec >> Expect.equal (Ok value)
        ]
        value


basicTests : List Test
basicTests =
    [ describe "Codec.string"
        [ roundtrips Fuzz.string S.string SerializeV1.string
        ]
    , describe "Codec.string with unicode chars" [ roundtrips (Fuzz.constant "Ⓐ弈😀") S.string SerializeV1.string ]
    , describe "Codec.int"
        [ roundtrips maxRangeIntFuzz S.int SerializeV1.int
        ]
    , describe "Codec.float64"
        [ roundtrips Fuzz.float S.float SerializeV1.float
        ]
    , describe "Codec.bool"
        [ roundtrips Fuzz.bool S.bool SerializeV1.bool
        ]

    --, describe "Codec.char"
    --    [ roundtrips charFuzz S.char
    --    ]
    , describe "Codec.bytes"
        [ roundtrips fuzzBytes S.bytes SerializeV1.bytes
        ]
    , describe "Codec.byte"
        [ roundtrips (Fuzz.intRange 0 255) S.byte SerializeV1.byte
        ]
    , Test.fuzz Fuzz.int "Codec.byte with value outside of 0-255 range wrap around for encodeToBytes" <|
        \value ->
            S.encodeToBytes S.byte value
                |> S.decodeFromBytes S.byte
                |> Expect.equal (Ok (modBy 256 value))
    , Test.fuzz Fuzz.int "Codec.byte with value outside of 0-255 range wrap around for encodeToJson" <|
        \value ->
            S.encodeToJson S.byte value
                |> S.decodeFromJson S.byte
                |> Expect.equal (Ok (modBy 256 value))
    , test "Codec.unit" <|
        \_ -> roundtripHelper S.unit SerializeV1.unit ()
    ]


fuzzBytes : Fuzzer Bytes
fuzzBytes =
    Fuzz.list maxRangeIntFuzz |> Fuzz.map (List.map (Bytes.Encode.unsignedInt32 Bytes.LE) >> Bytes.Encode.sequence >> Bytes.Encode.encode)


containersTests : List Test
containersTests =
    [ describe "Codec.array"
        [ roundtrips (Fuzz.array maxRangeIntFuzz) (S.array S.int) (SerializeV1.array SerializeV1.int)
        ]
    , describe "Codec.list"
        [ roundtrips (Fuzz.list maxRangeIntFuzz) (S.list S.int) (SerializeV1.list SerializeV1.int)
        ]
    , describe "Codec.dict"
        [ roundtrips
            (Fuzz.map2 Tuple.pair Fuzz.string maxRangeIntFuzz
                |> Fuzz.list
                |> Fuzz.map Dict.fromList
            )
            (S.dict S.string S.int)
            (SerializeV1.dict SerializeV1.string SerializeV1.int)
        ]
    , describe "Codec.set"
        [ roundtrips
            (Fuzz.list maxRangeIntFuzz |> Fuzz.map Set.fromList)
            (S.set S.int)
            (SerializeV1.set SerializeV1.int)
        ]
    , describe "Codec.tuple"
        [ roundtrips
            (Fuzz.map2 Tuple.pair maxRangeIntFuzz maxRangeIntFuzz)
            (S.tuple S.int S.int)
            (SerializeV1.tuple SerializeV1.int SerializeV1.int)
        ]
    ]


maxRangeIntFuzz : Fuzzer Int
maxRangeIntFuzz =
    Fuzz.intRange Basics.Extra.minSafeInteger Basics.Extra.maxSafeInteger


charFuzz : Fuzzer Char
charFuzz =
    [ '😀', 'ß', Char.toUpper 'ß', 'a', '吧', '吗', '\t' ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


objectTests : List Test
objectTests =
    [ describe "with 0 fields"
        [ roundtrips (Fuzz.constant {})
            (S.record {}
                |> S.finishRecord
            )
            (SerializeV1.record {}
                |> SerializeV1.finishRecord
            )
        ]
    , describe "with 1 field"
        [ roundtrips (Fuzz.map (\i -> { fname = i }) maxRangeIntFuzz)
            (S.record (\i -> { fname = i })
                |> S.field .fname S.int
                |> S.finishRecord
            )
            (SerializeV1.record (\i -> { fname = i })
                |> SerializeV1.field .fname SerializeV1.int
                |> SerializeV1.finishRecord
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
            (S.record
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                |> S.field .a S.int
                |> S.field .b S.int
                |> S.finishRecord
            )
            (SerializeV1.record
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                |> SerializeV1.field .a SerializeV1.int
                |> SerializeV1.field .b SerializeV1.int
                |> SerializeV1.finishRecord
            )
        ]
    , test "nested record" <|
        \_ ->
            roundtripHelper
                (S.record
                    (\a b ->
                        { a = a
                        , b = b
                        }
                    )
                    |> S.field .a
                        (S.record
                            (\a b ->
                                { a = a
                                , b = b
                                }
                            )
                            |> S.field .a S.int
                            |> S.field .b S.int
                            |> S.finishRecord
                        )
                    |> S.field .b
                        (S.record
                            (\a b ->
                                { a = a
                                , b = b
                                }
                            )
                            |> S.field .a S.string
                            |> S.field .b S.int
                            |> S.finishRecord
                        )
                    |> S.finishRecord
                )
                (SerializeV1.record
                    (\a b ->
                        { a = a
                        , b = b
                        }
                    )
                    |> SerializeV1.field .a
                        (SerializeV1.record
                            (\a b ->
                                { a = a
                                , b = b
                                }
                            )
                            |> SerializeV1.field .a SerializeV1.int
                            |> SerializeV1.field .b SerializeV1.int
                            |> SerializeV1.finishRecord
                        )
                    |> SerializeV1.field .b
                        (SerializeV1.record
                            (\a b ->
                                { a = a
                                , b = b
                                }
                            )
                            |> SerializeV1.field .a SerializeV1.string
                            |> SerializeV1.field .b SerializeV1.int
                            |> SerializeV1.finishRecord
                        )
                    |> SerializeV1.finishRecord
                )
                { a = { a = 5, b = 3 }, b = { a = "test", b = 6 } }
    ]


customTests : List Test
customTests =
    [ describe "with 1 ctor, 0 args"
        [ roundtrips (Fuzz.constant ())
            (S.customType
                (\f v ->
                    case v of
                        () ->
                            f
                )
                |> S.variant0 ()
                |> S.finishCustomType
            )
            (SerializeV1.customType
                (\f v ->
                    case v of
                        () ->
                            f
                )
                |> SerializeV1.variant0 ()
                |> SerializeV1.finishCustomType
            )
        ]
    , test "with 1 ctor, 1 arg" <|
        \_ ->
            roundtripHelper
                (S.customType
                    (\f v ->
                        case v of
                            T1 a ->
                                f a
                    )
                    |> S.variant1 T1 S.int
                    |> S.finishCustomType
                )
                (SerializeV1.customType
                    (\f v ->
                        case v of
                            T1 a ->
                                f a
                    )
                    |> SerializeV1.variant1 T1 SerializeV1.int
                    |> SerializeV1.finishCustomType
                )
                (T1 6)
    , test "with 1 ctor, 2 arg" <|
        \_ ->
            roundtripHelper
                (S.customType
                    (\function v ->
                        case v of
                            T2 a b ->
                                function a b
                    )
                    |> S.variant2 T2 S.int S.int
                    |> S.finishCustomType
                )
                (SerializeV1.customType
                    (\function v ->
                        case v of
                            T2 a b ->
                                function a b
                    )
                    |> SerializeV1.variant2 T2 SerializeV1.int SerializeV1.int
                    |> SerializeV1.finishCustomType
                )
                (T2 10 11)
    , test "with 1 ctor, 3 arg" <|
        \_ ->
            roundtripHelper
                (S.customType
                    (\function v ->
                        case v of
                            T3 a b c ->
                                function a b c
                    )
                    |> S.variant3 T3 S.int S.int S.int
                    |> S.finishCustomType
                )
                (SerializeV1.customType
                    (\function v ->
                        case v of
                            T3 a b c ->
                                function a b c
                    )
                    |> SerializeV1.variant3 T3 SerializeV1.int SerializeV1.int SerializeV1.int
                    |> SerializeV1.finishCustomType
                )
                (T3 10 11 12)
    , test "with 1 ctor, 4 arg" <|
        \_ ->
            roundtripHelper
                (S.customType
                    (\function v ->
                        case v of
                            T4 a b c d ->
                                function a b c d
                    )
                    |> S.variant4 T4 S.int S.int S.int S.int
                    |> S.finishCustomType
                )
                (SerializeV1.customType
                    (\function v ->
                        case v of
                            T4 a b c d ->
                                function a b c d
                    )
                    |> SerializeV1.variant4 T4 SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int
                    |> SerializeV1.finishCustomType
                )
                (T4 10 11 12 13)
    , test "with 1 ctor, 5 arg" <|
        \_ ->
            roundtripHelper
                (S.customType
                    (\function v ->
                        case v of
                            T5 a b c d e ->
                                function a b c d e
                    )
                    |> S.variant5 T5 S.int S.int S.int S.int S.int
                    |> S.finishCustomType
                )
                (SerializeV1.customType
                    (\function v ->
                        case v of
                            T5 a b c d e ->
                                function a b c d e
                    )
                    |> SerializeV1.variant5 T5 SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int
                    |> SerializeV1.finishCustomType
                )
                (T5 10 11 12 13 14)
    , test "with 1 ctor, 6 arg" <|
        \_ ->
            roundtripHelper
                (S.customType
                    (\function v ->
                        case v of
                            T6 a b c d e f ->
                                function a b c d e f
                    )
                    |> S.variant6 T6 S.int S.int S.int S.int S.int S.int
                    |> S.finishCustomType
                )
                (SerializeV1.customType
                    (\function v ->
                        case v of
                            T6 a b c d e f ->
                                function a b c d e f
                    )
                    |> SerializeV1.variant6 T6 SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int
                    |> SerializeV1.finishCustomType
                )
                (T6 10 11 12 13 14 15)
    , test "with 1 ctor, 7 arg" <|
        \_ ->
            roundtripHelper
                (S.customType
                    (\function v ->
                        case v of
                            T7 a b c d e f g ->
                                function a b c d e f g
                    )
                    |> S.variant7 T7 S.int S.int S.int S.int S.int S.int S.int
                    |> S.finishCustomType
                )
                (SerializeV1.customType
                    (\function v ->
                        case v of
                            T7 a b c d e f g ->
                                function a b c d e f g
                    )
                    |> SerializeV1.variant7 T7 SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int
                    |> SerializeV1.finishCustomType
                )
                (T7 10 11 12 13 14 15 16)
    , test "with 1 ctor, 8 arg" <|
        \_ ->
            roundtripHelper
                (S.customType
                    (\function v ->
                        case v of
                            T8 a b c d e f g h ->
                                function a b c d e f g h
                    )
                    |> S.variant8 T8 S.int S.int S.int S.int S.int S.int S.int S.int
                    |> S.finishCustomType
                )
                (SerializeV1.customType
                    (\function v ->
                        case v of
                            T8 a b c d e f g h ->
                                function a b c d e f g h
                    )
                    |> SerializeV1.variant8 T8 SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int SerializeV1.int
                    |> SerializeV1.finishCustomType
                )
                (T8 10 11 12 13 14 15 16 17)
    , describe "with 2 ctors, 0,1 args" <|
        let
            match fnothing fjust value =
                case value of
                    Nothing ->
                        fnothing

                    Just v ->
                        fjust v

            codec =
                S.customType match
                    |> S.variant0 Nothing
                    |> S.variant1 Just S.int
                    |> S.finishCustomType

            codecV1 =
                SerializeV1.customType match
                    |> SerializeV1.variant0 Nothing
                    |> SerializeV1.variant1 Just SerializeV1.int
                    |> SerializeV1.finishCustomType

            fuzzers =
                [ ( "1st ctor", Fuzz.constant Nothing )
                , ( "2nd ctor", Fuzz.map Just maxRangeIntFuzz )
                ]
        in
        fuzzers
            |> List.map
                (\( name, fuzz ) ->
                    describe name
                        [ roundtrips fuzz codec codecV1 ]
                )
    ]


bimapTests : List Test
bimapTests =
    [ roundtrips Fuzz.float
        (S.map
            (\x -> x * 2)
            (\x -> x / 2)
            S.float
        )
        (SerializeV1.map
            (\x -> x * 2)
            (\x -> x / 2)
            SerializeV1.float
        )
    ]


{-| Volume must be between 0 and 1.
-}
volumeCodec : Codec String Float
volumeCodec =
    S.float
        |> S.mapValid
            (\volume ->
                if volume <= 1 && volume >= 0 then
                    Ok volume

                else
                    Err ("Volume is outside of valid range. Value: " ++ String.fromFloat volume)
            )
            (\volume -> volume)


volumeCodecV1 : SerializeV1.Codec String Float
volumeCodecV1 =
    SerializeV1.float
        |> SerializeV1.mapValid
            (\volume ->
                if volume <= 1 && volume >= 0 then
                    Ok volume

                else
                    Err ("Volume is outside of valid range. Value: " ++ String.fromFloat volume)
            )
            (\volume -> volume)


andThenTests : List Test
andThenTests =
    [ roundtrips (Fuzz.floatRange 0 1) volumeCodec volumeCodecV1
    , test "andThen fails on invalid binary data." <|
        \_ ->
            5
                |> S.encodeToBytes volumeCodec
                |> S.decodeFromBytes volumeCodec
                |> Expect.equal (S.CustomError "Volume is outside of valid range. Value: 5" |> Err)
    ]


type alias Record =
    { a : Int
    , b : Float
    , c : String
    , d : ()
    , e : Float
    }


errorTests : List Test
errorTests =
    [ test "variant produces correct error message." <|
        \_ ->
            let
                codec =
                    S.customType
                        (\encodeNothing encodeJust value ->
                            case value of
                                Nothing ->
                                    encodeNothing

                                Just v ->
                                    encodeJust v
                        )
                        |> S.variant0 Nothing
                        |> S.variant1 Just S.int
                        |> S.finishCustomType

                codecBad =
                    S.customType
                        (\encodeNothing _ encodeJust value ->
                            case value of
                                Nothing ->
                                    encodeNothing

                                Just v ->
                                    encodeJust v
                        )
                        |> S.variant0 Nothing
                        |> S.variant0 Nothing
                        |> S.variant1 Just S.int
                        |> S.finishCustomType
            in
            S.encodeToBytes codecBad (Just 0) |> S.decodeFromBytes codec |> Expect.equal (Err S.DataCorrupted)
    , test "list produces correct error message." <|
        \_ ->
            let
                codec =
                    S.list volumeCodec
            in
            S.encodeToBytes codec [ 0, 3, 0, 4, 0, 0 ]
                |> S.decodeFromBytes codec
                |> Expect.equal
                    (Err <| S.CustomError "Volume is outside of valid range. Value: 3")
    , test "Record produces correct error message." <|
        \_ ->
            let
                codec =
                    S.record Record
                        |> S.field .a S.int
                        |> S.field .b volumeCodec
                        |> S.field .c S.string
                        |> S.field .d S.unit
                        |> S.field .e volumeCodec
                        |> S.finishRecord
            in
            S.encodeToBytes codec { a = 0, b = 0, c = "", d = (), e = -1 }
                |> S.decodeFromBytes codec
                |> Expect.equal
                    (Err <| S.CustomError "Volume is outside of valid range. Value: -1")
    , test "Record produces first error message." <|
        \_ ->
            let
                codec =
                    S.record Record
                        |> S.field .a S.int
                        |> S.field .b volumeCodec
                        |> S.field .c S.string
                        |> S.field .d S.unit
                        |> S.field .e volumeCodec
                        |> S.finishRecord
            in
            S.encodeToBytes codec { a = 0, b = -2, c = "", d = (), e = -3 }
                |> S.decodeFromBytes codec
                |> Expect.equal
                    (Err <| S.CustomError "Volume is outside of valid range. Value: -2")
    , test "Map error message." <|
        \_ ->
            let
                codec =
                    S.record Record
                        |> S.field .a S.int
                        |> S.field .b volumeCodec
                        |> S.field .c S.string
                        |> S.field .d S.unit
                        |> S.field .e volumeCodec
                        |> S.finishRecord
                        |> S.mapError (\text -> "Error in Record: " ++ text)
            in
            S.encodeToBytes codec { a = 0, b = -2, c = "", d = (), e = -3 }
                |> S.decodeFromBytes codec
                |> Expect.equal
                    (Err <| S.CustomError "Error in Record: Volume is outside of valid range. Value: -2")
    ]


type Peano
    = Peano (Maybe Peano)


{-| This is the same example used in Codec.recursive but adapted for lazy.
-}
peanoCodec : Codec e Peano
peanoCodec =
    S.maybe (S.lazy (\() -> peanoCodec)) |> S.map Peano (\(Peano a) -> a)


peanoCodecV1 : SerializeV1.Codec e Peano
peanoCodecV1 =
    SerializeV1.maybe (SerializeV1.lazy (\() -> peanoCodecV1)) |> SerializeV1.map Peano (\(Peano a) -> a)


lazyTests : List Test
lazyTests =
    [ roundtrips peanoFuzz peanoCodec peanoCodecV1
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
        [ roundtrips (maybeFuzz maxRangeIntFuzz) (S.maybe S.int) (SerializeV1.maybe SerializeV1.int)
        ]
    ]


maybeFuzz : Fuzzer a -> Fuzzer (Maybe a)
maybeFuzz fuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Nothing
        , Fuzz.map Just fuzzer
        ]


type DaysOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


daysOfWeekCodec : Codec e DaysOfWeek
daysOfWeekCodec =
    S.enum Monday [ Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]


daysOfWeekCodecV1 : SerializeV1.Codec e DaysOfWeek
daysOfWeekCodecV1 =
    SerializeV1.enum Monday [ Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]


badDaysOfWeekCodec : Codec e DaysOfWeek
badDaysOfWeekCodec =
    S.enum Monday []


daysOfWeekFuzz : Fuzzer DaysOfWeek
daysOfWeekFuzz =
    [ Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


enumTest : List Test
enumTest =
    [ roundtrips daysOfWeekFuzz daysOfWeekCodec daysOfWeekCodecV1
    , test "Default to first item when encoding if item doesn't exist." <|
        \_ ->
            S.encodeToBytes badDaysOfWeekCodec Tuesday |> S.decodeFromBytes badDaysOfWeekCodec |> Expect.equal (Ok Monday)
    , test "Error if enum index is greater than number of values in enum." <|
        \_ ->
            S.encodeToBytes daysOfWeekCodec Tuesday |> S.decodeFromBytes badDaysOfWeekCodec |> Expect.equal (Err S.DataCorrupted)
    ]


serializerVersionTests : List Test
serializerVersionTests =
    [ test "DataCorrupted error if version is 0" <|
        \_ ->
            Bytes.Encode.sequence [ Bytes.Encode.unsignedInt8 0, Bytes.Encode.unsignedInt8 5 ]
                |> Bytes.Encode.encode
                |> S.decodeFromBytes S.byte
                |> Expect.equal (Err S.DataCorrupted)
    , test "Ok result if version is 1" <|
        \_ ->
            Bytes.Encode.sequence [ Bytes.Encode.unsignedInt8 1, Bytes.Encode.unsignedInt8 5 ]
                |> Bytes.Encode.encode
                |> S.decodeFromBytes S.byte
                |> Expect.equal (Ok 5)
    , fuzz (Fuzz.intRange 2 255) "SerializerOutOfDate if version is greater than Serializer version" <|
        \version ->
            Bytes.Encode.sequence [ Bytes.Encode.unsignedInt8 version, Bytes.Encode.unsignedInt8 5 ]
                |> Bytes.Encode.encode
                |> S.decodeFromBytes S.byte
                |> Expect.equal (Err S.SerializerOutOfDate)
    ]
