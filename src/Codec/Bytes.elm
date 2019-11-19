module Codec.Bytes exposing
    ( Codec, Encoder, Error(..)
    , Decoder
    , string, bool, char, float64, float32, signedInt32, unsignedInt32, signedInt16, unsignedInt16, signedInt8, unsignedInt8, bytes
    , maybe, list, array, dict, set, tuple, triple, result
    , ObjectCodec, object, field, finishObject
    , CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8
    , map, andThen
    , constant, lazy
    , decode, encode, finishCustom, getDecoder, getEncoder
    )

{-| A `Codec a` contains a `Bytes.Decoder a` and the corresponding `a -> Bytes.Encoder`.


# Definition

@docs Codec, Encoder, Bytes, Error


# Decode

@docs Decoder, decoder, decodeValue


# Encode

@docs encoder, encodeToValue


# Primitives

@docs string, bool, char, float64, float32, signedInt32, unsignedInt32, signedInt16, unsignedInt16, signedInt8, unsignedInt8, bytes


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result


# Object Primitives

@docs ObjectCodec, object, field, finishObject


# Custom Types

@docs CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom


# Mapping

@docs map, andThen


# Fancy Codecs

@docs constant, lazy

-}

import Array exposing (Array)
import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Set exposing (Set)
import Toop exposing (T4(..), T5(..), T6(..), T7(..), T8(..))



-- DEFINITION


{-| A value that knows how to encode and decode a sequence of bytes.
-}
type Codec a
    = Codec
        { encoder : a -> Encoder
        , decoder : Decoder (Result Error a)
        }


type Error
    = BaseError String
    | CustomTypeError { customIndex : Int, variantIndex : Int, error : Error }
    | NoVariantMatches
    | RecordError { fieldIndex : Int, error : Error }
    | ListError { listIndex : Int, error : Error }
    | TupleError { tupleIndex : Int, error : Error }


errorToString : Error -> String
errorToString =
    Debug.todo ""


{-| Describes how to generate a sequence of bytes.
-}
type alias Encoder =
    BE.Encoder



-- DECODE


{-| Describes how to turn a sequence of bytes into a nice Elm value.
-}
type alias Decoder a =
    BD.Decoder a


endian : Bytes.Endianness
endian =
    Bytes.BE


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
getDecoder : Codec a -> Decoder (Result Error a)
getDecoder (Codec m) =
    m.decoder


{-| Run a `Codec` to turn a sequence of bytes into an Elm value.
-}
decode : Codec a -> Bytes.Bytes -> Result Error a
decode codec bytes_ =
    case BD.decode (getDecoder codec) bytes_ of
        Just value ->
            value

        Nothing ->
            BaseError "Unknown error" |> Err



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
getEncoder : Codec a -> a -> Encoder
getEncoder (Codec m) =
    m.encoder


{-| Convert an Elm value into a sequence of bytes.
-}
encode : Codec a -> a -> Bytes.Bytes
encode codec value =
    getEncoder codec value |> BE.encode



-- BASE


{-| If necessary you can create your own `Codec` directly.
This should be a measure of last resort though! If you need to encode and decode records and custom types, use `object` and `custom` respectively.
-}
build : (a -> Encoder) -> Decoder (Result Error a) -> Codec a
build encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        }


{-| `Codec` between a sequence of bytes and an Elm `String`
-}
string : Codec String
string =
    build
        (\text ->
            BE.sequence
                [ BE.unsignedInt32 endian (String.length text)
                , BE.string text
                ]
        )
        (BD.unsignedInt32 endian
            |> BD.andThen
                (\charCount -> BD.string charCount |> BD.map Ok)
        )


{-| `Codec` between a sequence of bytes and an Elm `Bool`
-}
bool : Codec Bool
bool =
    build
        (\value ->
            if value then
                BE.unsignedInt8 1

            else
                BE.unsignedInt8 0
        )
        (BD.unsignedInt8
            |> BD.andThen
                (\value ->
                    case value of
                        0 ->
                            BD.succeed (Ok False)

                        1 ->
                            BD.succeed (Ok True)

                        _ ->
                            ("Tried parsing a bool but the value " ++ String.fromInt value ++ " must equal 0 or 1.")
                                |> BaseError
                                |> Err
                                |> BD.succeed
                )
        )


{-| `Codec` between a signed 32-bit integer and an Elm `Int`
-}
signedInt32 : Codec Int
signedInt32 =
    build (BE.signedInt32 endian) (BD.signedInt32 endian |> BD.map Ok)


{-| `Codec` between an unsigned 32-bit integer and an Elm `Int`
-}
unsignedInt32 : Codec Int
unsignedInt32 =
    build (BE.unsignedInt32 endian) (BD.unsignedInt32 endian |> BD.map Ok)


{-| `Codec` between a signed 16-bit integer and an Elm `Int`
-}
signedInt16 : Codec Int
signedInt16 =
    build (BE.signedInt16 endian) (BD.signedInt16 endian |> BD.map Ok)


{-| `Codec` between an unsigned 16-bit integer and an Elm `Int`
-}
unsignedInt16 : Codec Int
unsignedInt16 =
    build (BE.unsignedInt16 endian) (BD.unsignedInt16 endian |> BD.map Ok)


{-| `Codec` between a signed 8-bit integer and an Elm `Int`
-}
signedInt8 : Codec Int
signedInt8 =
    build BE.signedInt8 (BD.signedInt8 |> BD.map Ok)


{-| `Codec` between an unsigned 8-bit integer and an Elm `Int`
-}
unsignedInt8 : Codec Int
unsignedInt8 =
    build BE.unsignedInt8 (BD.unsignedInt8 |> BD.map Ok)


{-| `Codec` between a 64-bit float and an Elm `Float`
-}
float64 : Codec Float
float64 =
    build (BE.float64 endian) (BD.float64 endian |> BD.map Ok)


{-| `Codec` between a 32-bit float and an Elm `Float`.
Due to Elm `Float`s being 64-bit, encoding and decoding it as a 32-bit float won't exactly equal the original value.
-}
float32 : Codec Float
float32 =
    build (BE.float32 endian) (BD.float32 endian |> BD.map Ok)


{-| `Codec` between a sequence of bytes and an Elm `Char`
-}
char : Codec Char
char =
    let
        charEncode text =
            BE.sequence
                [ BE.unsignedInt32 endian (String.length text)
                , BE.string text
                ]
    in
    build
        (String.fromChar >> charEncode)
        (BD.unsignedInt32 endian
            |> BD.andThen (\charCount -> BD.string charCount)
            |> BD.map
                (\text ->
                    case String.toList text |> List.head of
                        Just char_ ->
                            Ok char_

                        Nothing ->
                            "Tried to parse a char but failed" |> BaseError |> Err
                )
        )



-- DATA STRUCTURES


{-| Represents an optional value.
-}
maybe : Codec a -> Codec (Maybe a)
maybe codec =
    custom
        (\nothingEncoder justEncoder value ->
            case value of
                Nothing ->
                    nothingEncoder

                Just value_ ->
                    justEncoder value_
        )
        |> variant0 Nothing
        |> variant1 Just codec
        |> finishCustom


{-| `Codec` between a sequence of bytes and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list codec =
    Codec
        { encoder = listEncode (getEncoder codec)
        , decoder =
            BD.unsignedInt32 endian
                |> BD.andThen
                    (\length -> BD.loop ( length, [] ) (listStep (getDecoder codec)))
        }


listEncode : (a -> Encoder) -> List a -> Encoder
listEncode encoder_ list_ =
    list_
        |> List.map encoder_
        |> List.reverse
        |> (::) (BE.unsignedInt32 endian (List.length list_))
        |> BE.sequence


listStep : BD.Decoder (Result Error a) -> ( Int, List a ) -> Decoder (BD.Step ( Int, List a ) (Result Error (List a)))
listStep decoder_ ( n, xs ) =
    if n <= 0 then
        BD.succeed (BD.Done (Ok xs))

    else
        BD.map
            (\x ->
                case x of
                    Ok ok ->
                        BD.Loop ( n - 1, ok :: xs )

                    Err err ->
                        BD.Done (ListError { listIndex = n, error = err } |> Err)
            )
            decoder_


{-| `Codec` between a sequence of bytes and an Elm `Array`.
-}
array : Codec a -> Codec (Array a)
array codec =
    list codec |> map Array.fromList Array.toList


{-| `Codec` between a sequence of bytes and an Elm `Dict`.
-}
dict : Codec comparable -> Codec a -> Codec (Dict comparable a)
dict keyCodec valueCodec =
    list (tuple keyCodec valueCodec) |> map Dict.fromList Dict.toList


{-| `Codec` between a sequence of bytes and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set comparable)
set codec =
    list codec |> map Set.fromList Set.toList


{-| `Codec` between a sequence of bytes and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple m1 m2 =
    Codec
        { encoder =
            \( v1, v2 ) ->
                BE.sequence
                    [ getEncoder m1 v1
                    , getEncoder m2 v2
                    ]
        , decoder =
            BD.map2
                (\a b ->
                    case ( a, b ) of
                        ( Ok aOk, Ok bOk ) ->
                            Ok ( aOk, bOk )

                        ( Err aError, _ ) ->
                            TupleError { tupleIndex = 0, error = aError } |> Err

                        ( _, Err bError ) ->
                            TupleError { tupleIndex = 1, error = bError } |> Err
                )
                (getDecoder m1)
                (getDecoder m2)
        }


{-| `Codec` between a sequence of bytes and an Elm triple.
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple m1 m2 m3 =
    Codec
        { encoder =
            \( v1, v2, v3 ) ->
                BE.sequence
                    [ getEncoder m1 v1
                    , getEncoder m2 v2
                    , getEncoder m3 v3
                    ]
        , decoder =
            BD.map3
                (\a b c ->
                    case ( a, b, c ) of
                        ( Ok aOk, Ok bOk, Ok cOk ) ->
                            Ok ( aOk, bOk, cOk )

                        ( Err aError, _, _ ) ->
                            TupleError { tupleIndex = 0, error = aError } |> Err

                        ( _, Err bError, _ ) ->
                            TupleError { tupleIndex = 1, error = bError } |> Err

                        ( _, _, Err cError ) ->
                            TupleError { tupleIndex = 2, error = cError } |> Err
                )
                (getDecoder m1)
                (getDecoder m2)
                (getDecoder m3)
        }


{-| `Codec` for `Result` values.
-}
result : Codec error -> Codec value -> Codec (Result error value)
result errorCodec valueCodec =
    custom
        (\errEncoder okEncoder value ->
            case value of
                Err err ->
                    errEncoder err

                Ok ok ->
                    okEncoder ok
        )
        |> variant1 Err errorCodec
        |> variant1 Ok valueCodec
        |> finishCustom


{-| `Codec` for `Bytes`. This is useful if you wanted to include binary data that you're going to decode elsewhere.

    pngCodec =
        Codec.bytes |> Codec.map pngEncoder pngDecoder

-}
bytes : Codec Bytes.Bytes
bytes =
    Codec
        { encoder =
            \bytes_ ->
                BE.sequence
                    [ BE.unsignedInt32 endian (Bytes.width bytes_)
                    , BE.bytes bytes_
                    ]
        , decoder = BD.unsignedInt32 endian |> BD.andThen (\length -> BD.bytes length |> BD.map Ok)
        }



-- OBJECTS


{-| A partially built `Codec` for an object.
-}
type ObjectCodec a b
    = ObjectCodec
        { encoder : a -> List Encoder
        , decoder : Decoder (Result Error b)
        , fieldCount : Int
        }


{-| Start creating a `Codec` for an object. You should pass the constructor as the first parameter.
If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an object.

    type alias Point =
        { x : Int
        , y : Int
        }

    pointCodec : Codec Point
    pointCodec =
        Codec.object Point
            -- Note that adding, removing, or reordering fields will prevent you from decoding existing data.
            |> Codec.field .x Codec.signedInt
            |> Codec.field .y Codec.signedInt
            |> Codec.finishObject

-}
object : b -> ObjectCodec a b
object ctor =
    ObjectCodec
        { encoder = \_ -> []
        , decoder = BD.succeed (Ok ctor)
        , fieldCount = 0
        }


{-| Specify how to get a value from the object we want to encode and then give a `Codec` for that value.
-}
field : (a -> f) -> Codec f -> ObjectCodec a (f -> b) -> ObjectCodec a b
field getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder = \v -> (getEncoder codec <| getter v) :: ocodec.encoder v
        , decoder =
            BD.map2
                (\f x ->
                    case ( f, x ) of
                        ( Ok fOk, Ok xOk ) ->
                            fOk xOk |> Ok

                        ( Err fError, _ ) ->
                            Err fError

                        ( _, Err xError ) ->
                            RecordError { fieldIndex = ocodec.fieldCount, error = xError } |> Err
                )
                ocodec.decoder
                (getDecoder codec)
        , fieldCount = ocodec.fieldCount + 1
        }


{-| Create a `Codec` from a fully specified `ObjectCodec`.
-}
finishObject : ObjectCodec a a -> Codec a
finishObject (ObjectCodec om) =
    Codec
        { encoder = om.encoder >> List.reverse >> BE.sequence
        , decoder = om.decoder
        }



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomCodec match v
    = CustomCodec
        { match : match
        , decoder : Int -> Decoder (Result Error v) -> Decoder (Result Error v)
        , idCounter : Int
        }


{-| Starts building a `Codec` for a custom type.
You need to pass a pattern matching function, see the FAQ for details.

    type Semaphore
        = Red Int String Bool
        | Yellow Float
        | Green

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        Codec.custom
            (\redEncoder yellowEncoder greenEncoder value ->
                case value of
                    Red i s b ->
                        redEncoder i s b

                    Yellow f ->
                        yellowEncoder f

                    Green ->
                        greenEncoder
            )
            -- Note that removing a variant, inserting a variant before an existing one, or swapping two variants will prevent you from decoding existing data.
            |> Codec.variant3 Red Codec.signedInt Codec.string Codec.bool
            |> Codec.variant1 Yellow Codec.float64
            |> Codec.variant0 Green
            -- It's safe to add new variants here later though
            |> Codec.finishCustom

-}
custom : match -> CustomCodec match value
custom match =
    CustomCodec
        { match = match
        , decoder = \_ -> identity
        , idCounter = 0
        }


variant :
    ((List Encoder -> Encoder) -> a)
    -> (Int -> Decoder (Result Error v))
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
variant matchPiece decoderPiece (CustomCodec am) =
    let
        enc v =
            BE.unsignedInt16 endian am.idCounter
                :: v
                |> BE.sequence

        decoder_ tag orElse =
            if tag == am.idCounter then
                decoderPiece am.idCounter

            else
                am.decoder tag orElse
    in
    CustomCodec
        { match = am.match <| matchPiece enc
        , decoder = decoder_
        , idCounter = am.idCounter + 1
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 : v -> CustomCodec (Encoder -> a) v -> CustomCodec a v
variant0 ctor =
    variant
        (\c -> c [])
        (\_ -> BD.succeed (Ok ctor))


variantError customIndex variantIndex error =
    CustomTypeError { customIndex = customIndex, variantIndex = variantIndex, error = error } |> Err


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    (a -> v)
    -> Codec a
    -> CustomCodec ((a -> Encoder) -> b) v
    -> CustomCodec b v
variant1 ctor m1 =
    variant
        (\c v ->
            c
                [ getEncoder m1 v
                ]
        )
        (\index ->
            BD.map
                (\value ->
                    case value of
                        Ok ok ->
                            ctor ok |> Ok

                        Err err ->
                            variantError index 0 err
                )
                (getDecoder m1)
        )


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    (a -> b -> v)
    -> Codec a
    -> Codec b
    -> CustomCodec ((a -> b -> Encoder) -> c) v
    -> CustomCodec c v
variant2 ctor m1 m2 =
    variant
        (\c v1 v2 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                ]
        )
        (\index ->
            BD.map2
                (\v1 v2 ->
                    case ( v1, v2 ) of
                        ( Ok ok1, Ok ok2 ) ->
                            ctor ok1 ok2 |> Ok

                        ( Err err, _ ) ->
                            variantError index 0 err

                        ( _, Err err ) ->
                            variantError index 1 err
                )
                (getDecoder m1)
                (getDecoder m2)
        )


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    (a -> b -> c -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> CustomCodec ((a -> b -> c -> Encoder) -> partial) v
    -> CustomCodec partial v
variant3 ctor m1 m2 m3 =
    variant
        (\c v1 v2 v3 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                , getEncoder m3 v3
                ]
        )
        (\index ->
            BD.map3
                (\v1 v2 v3 ->
                    case ( v1, v2, v3 ) of
                        ( Ok ok1, Ok ok2, Ok ok3 ) ->
                            ctor ok1 ok2 ok3 |> Ok

                        ( Err err, _, _ ) ->
                            variantError index 0 err

                        ( _, Err err, _ ) ->
                            variantError index 1 err

                        ( _, _, Err err ) ->
                            variantError index 2 err
                )
                (getDecoder m1)
                (getDecoder m2)
                (getDecoder m3)
        )


{-| Define a variant with 4 parameters for a custom type.
-}
variant4 :
    (a -> b -> c -> d -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> CustomCodec ((a -> b -> c -> d -> Encoder) -> partial) v
    -> CustomCodec partial v
variant4 ctor m1 m2 m3 m4 =
    variant
        (\c v1 v2 v3 v4 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                , getEncoder m3 v3
                , getEncoder m4 v4
                ]
        )
        (\index ->
            BD.map4
                (\v1 v2 v3 v4 ->
                    case T4 v1 v2 v3 v4 of
                        T4 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) ->
                            ctor ok1 ok2 ok3 ok4 |> Ok

                        T4 (Err err) _ _ _ ->
                            variantError index 0 err

                        T4 _ (Err err) _ _ ->
                            variantError index 1 err

                        T4 _ _ (Err err) _ ->
                            variantError index 2 err

                        T4 _ _ _ (Err err) ->
                            variantError index 3 err
                )
                (getDecoder m1)
                (getDecoder m2)
                (getDecoder m3)
                (getDecoder m4)
        )


{-| Define a variant with 5 parameters for a custom type.
-}
variant5 :
    (a -> b -> c -> d -> e -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> CustomCodec ((a -> b -> c -> d -> e -> Encoder) -> partial) v
    -> CustomCodec partial v
variant5 ctor m1 m2 m3 m4 m5 =
    variant
        (\c v1 v2 v3 v4 v5 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                , getEncoder m3 v3
                , getEncoder m4 v4
                , getEncoder m5 v5
                ]
        )
        (\index ->
            BD.map5
                (\v1 v2 v3 v4 v5 ->
                    case T5 v1 v2 v3 v4 v5 of
                        T5 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) ->
                            ctor ok1 ok2 ok3 ok4 ok5 |> Ok

                        T5 (Err err) _ _ _ _ ->
                            variantError index 0 err

                        T5 _ (Err err) _ _ _ ->
                            variantError index 1 err

                        T5 _ _ (Err err) _ _ ->
                            variantError index 2 err

                        T5 _ _ _ (Err err) _ ->
                            variantError index 3 err

                        T5 _ _ _ _ (Err err) ->
                            variantError index 4 err
                )
                (getDecoder m1)
                (getDecoder m2)
                (getDecoder m3)
                (getDecoder m4)
                (getDecoder m5)
        )


{-| Define a variant with 6 parameters for a custom type.
-}
variant6 :
    (a -> b -> c -> d -> e -> f -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> Encoder) -> partial) v
    -> CustomCodec partial v
variant6 ctor m1 m2 m3 m4 m5 m6 =
    variant
        (\c v1 v2 v3 v4 v5 v6 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                , getEncoder m3 v3
                , getEncoder m4 v4
                , getEncoder m5 v5
                , getEncoder m6 v6
                ]
        )
        (\index ->
            BD.map5
                (\v1 v2 v3 v4 ( v5, v6 ) ->
                    case T6 v1 v2 v3 v4 v5 v6 of
                        T6 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) ->
                            ctor ok1 ok2 ok3 ok4 ok5 ok6 |> Ok

                        T6 (Err err) _ _ _ _ _ ->
                            variantError index 0 err

                        T6 _ (Err err) _ _ _ _ ->
                            variantError index 1 err

                        T6 _ _ (Err err) _ _ _ ->
                            variantError index 2 err

                        T6 _ _ _ (Err err) _ _ ->
                            variantError index 3 err

                        T6 _ _ _ _ (Err err) _ ->
                            variantError index 4 err

                        T6 _ _ _ _ _ (Err err) ->
                            variantError index 5 err
                )
                (getDecoder m1)
                (getDecoder m2)
                (getDecoder m3)
                (getDecoder m4)
                (BD.map2 Tuple.pair
                    (getDecoder m5)
                    (getDecoder m6)
                )
        )


{-| Define a variant with 7 parameters for a custom type.
-}
variant7 :
    (a -> b -> c -> d -> e -> f -> g -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> Encoder) -> partial) v
    -> CustomCodec partial v
variant7 ctor m1 m2 m3 m4 m5 m6 m7 =
    variant
        (\c v1 v2 v3 v4 v5 v6 v7 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                , getEncoder m3 v3
                , getEncoder m4 v4
                , getEncoder m5 v5
                , getEncoder m6 v6
                , getEncoder m7 v7
                ]
        )
        (\index ->
            BD.map5
                (\v1 v2 v3 ( v4, v5 ) ( v6, v7 ) ->
                    case T7 v1 v2 v3 v4 v5 v6 v7 of
                        T7 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) (Ok ok7) ->
                            ctor ok1 ok2 ok3 ok4 ok5 ok6 ok7 |> Ok

                        T7 (Err err) _ _ _ _ _ _ ->
                            variantError index 0 err

                        T7 _ (Err err) _ _ _ _ _ ->
                            variantError index 1 err

                        T7 _ _ (Err err) _ _ _ _ ->
                            variantError index 2 err

                        T7 _ _ _ (Err err) _ _ _ ->
                            variantError index 3 err

                        T7 _ _ _ _ (Err err) _ _ ->
                            variantError index 4 err

                        T7 _ _ _ _ _ (Err err) _ ->
                            variantError index 5 err

                        T7 _ _ _ _ _ _ (Err err) ->
                            variantError index 6 err
                )
                (getDecoder m1)
                (getDecoder m2)
                (getDecoder m3)
                (BD.map2 Tuple.pair
                    (getDecoder m4)
                    (getDecoder m5)
                )
                (BD.map2 Tuple.pair
                    (getDecoder m6)
                    (getDecoder m7)
                )
        )


{-| Define a variant with 8 parameters for a custom type.
-}
variant8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> Codec h
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> h -> Encoder) -> partial) v
    -> CustomCodec partial v
variant8 ctor m1 m2 m3 m4 m5 m6 m7 m8 =
    variant
        (\c v1 v2 v3 v4 v5 v6 v7 v8 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                , getEncoder m3 v3
                , getEncoder m4 v4
                , getEncoder m5 v5
                , getEncoder m6 v6
                , getEncoder m7 v7
                , getEncoder m8 v8
                ]
        )
        (\index ->
            BD.map5
                (\v1 v2 ( v3, v4 ) ( v5, v6 ) ( v7, v8 ) ->
                    case T8 v1 v2 v3 v4 v5 v6 v7 v8 of
                        T8 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) (Ok ok7) (Ok ok8) ->
                            ctor ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 |> Ok

                        T8 (Err err) _ _ _ _ _ _ _ ->
                            variantError index 0 err

                        T8 _ (Err err) _ _ _ _ _ _ ->
                            variantError index 1 err

                        T8 _ _ (Err err) _ _ _ _ _ ->
                            variantError index 2 err

                        T8 _ _ _ (Err err) _ _ _ _ ->
                            variantError index 3 err

                        T8 _ _ _ _ (Err err) _ _ _ ->
                            variantError index 4 err

                        T8 _ _ _ _ _ (Err err) _ _ ->
                            variantError index 5 err

                        T8 _ _ _ _ _ _ (Err err) _ ->
                            variantError index 6 err

                        T8 _ _ _ _ _ _ _ (Err err) ->
                            variantError index 7 err
                )
                (getDecoder m1)
                (getDecoder m2)
                (BD.map2 Tuple.pair
                    (getDecoder m3)
                    (getDecoder m4)
                )
                (BD.map2 Tuple.pair
                    (getDecoder m5)
                    (getDecoder m6)
                )
                (BD.map2 Tuple.pair
                    (getDecoder m7)
                    (getDecoder m8)
                )
        )


{-| Build a `Codec` for a fully specified custom type.
-}
finishCustom : CustomCodec (a -> Encoder) a -> Codec a
finishCustom (CustomCodec am) =
    Codec
        { encoder = \v -> am.match v
        , decoder =
            BD.unsignedInt16 endian
                |> BD.andThen
                    (\tag ->
                        am.decoder tag (BD.succeed (Err NoVariantMatches))
                    )
        }



---- MAPPING


{-| Transform a `Codec`.
-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map fromBytes toBytes codec =
    Codec
        { decoder =
            getDecoder codec
                |> BD.map
                    (\value ->
                        case value of
                            Ok ok ->
                                fromBytes ok |> Ok

                            Err err ->
                                Err err
                    )
        , encoder = \v -> toBytes v |> getEncoder codec
        }


{-| Transform a `Codec` in a way that can potentially fail when decoding.

    {-| Volume must be between 0 and 1.
    -}
    volumeCodec =
        Codec.float64
            |> Codec.andThen
                (\volume ->
                    if volume <= 1 && volume >= 0 then
                        Ok volume

                    else
                        Err "Volume is outside of valid range."
                )
                (\volume -> volume)

-}
andThen : (a -> Result String b) -> (b -> a) -> Codec a -> Codec b
andThen fromBytes toBytes codec =
    Codec
        { decoder =
            getDecoder codec
                |> BD.map
                    (\value ->
                        case value of
                            Ok ok ->
                                fromBytes ok |> Result.mapError BaseError

                            Err err ->
                                Err err
                    )
        , encoder = \v -> toBytes v |> getEncoder codec
        }



-- FANCY


{-| Handle situations where you need to define a codec in terms of itself.

    type Peano
        = Peano (Maybe Peano)

    {-| The compiler will complain that this function causes an infinite loop.
    -}
    badPeanoCodec : Codec Peano
    badPeanoCodec =
        Codec.maybe badPeanoCodec |> Codec.map Peano (\(Peano a) -> a)

    {-| Now the compiler is happy!
    -}
    goodPeanoCodec : Codec Peano
    goodPeanoCodec =
        Codec.maybe (Codec.lazy (\() -> goodPeanoCodec)) |> Codec.map Peano (\(Peano a) -> a)

-}
lazy : (() -> Codec a) -> Codec a
lazy f =
    Codec
        { decoder = BD.succeed () |> BD.andThen (\() -> getDecoder (f ()))
        , encoder = \value -> getEncoder (f ()) value
        }


{-| Create a `Codec` that encodes nothing and always decodes as the same value.
-}
constant : a -> Codec a
constant default_ =
    Codec
        { decoder = BD.succeed (Ok default_)
        , encoder = \_ -> BE.sequence []
        }
