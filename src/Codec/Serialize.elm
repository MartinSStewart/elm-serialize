module Codec.Serialize exposing
    ( Codec, Error(..)
    , Decoder, getDecoder, fromBytes, fromString, errorToString
    , Encoder, getEncoder, toBytes, toString
    , string, bool, char, float, int, bytes
    , maybe, list, array, dict, set, tuple, triple, result, enum
    , RecordCodec, record, field, finishRecord
    , CustomTypeCodec, customType, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, finishCustomType
    , map, andThen
    , constant, lazy
    )

{-| A `Codec a` contains a `Bytes.Decoder a` and the corresponding `a -> Bytes.Encoder`.


# Definition

@docs Codec, Error


# Decode

@docs Decoder, getDecoder, fromBytes, fromString, errorToString


# Encode

@docs Encoder, getEncoder, toBytes, toString


# Primitives

@docs string, bool, char, float, int, bytes


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result, enum


# Records

@docs RecordCodec, record, field, finishRecord


# Custom Types

@docs CustomTypeCodec, customType, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, finishCustomType


# Mapping

@docs map, andThen


# Fancy Codecs

@docs constant, lazy

-}

import Array exposing (Array)
import Base64
import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Ordinal
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
    = AndThenCodecError String
    | EnumCodecNegativeIndex
    | EnumCodecValueNotFound
    | CharCodecError
    | BoolCodecError
    | DictCodecError
    | SetCodecError
    | MaybeCodecError
    | ResultCodecError
    | DataCorrupted
    | CustomTypeCodecError { variantIndex : Int, variantSize : Int, variantConstructorIndex : Int, error : Error }
    | NoVariantMatches
    | RecordCodecError { fieldIndex : Int, error : Error }
    | ListCodecError { listIndex : Int, error : Error }
    | ArrayCodecError { arrayIndex : Int, error : Error }
    | TupleCodecError { tupleIndex : Int, error : Error }
    | TripleCodecError { tripleIndex : Int, error : Error }


errorToString : Error -> String
errorToString errorData =
    let
        trace =
            "An error occured" |> errorToString_ errorData

        warning =
            ""
    in
    trace ++ "\n\n\n" ++ warning


errorToString_ : Error -> String -> String
errorToString_ errorData previousText =
    case errorData of
        AndThenCodecError text ->
            previousText ++ "The Codec.andThen returned this error message:\n    " ++ text

        DataCorrupted ->
            previousText ++ "Data corrupted. This probably means you tried reading in data that wasn't compatible with this codec."

        CustomTypeCodecError { variantIndex, variantSize, variantConstructorIndex, error } ->
            let
                variantText =
                    "|> variant"
                        ++ String.fromInt variantSize
                        ++ " _"
                        ++ String.repeat variantConstructorIndex " _"
                        ++ " x"
                        ++ String.repeat (variantSize - 1 - variantConstructorIndex) " codec"
            in
            previousText
                ++ " in a custom type codec, in the "
                ++ Ordinal.ordinal (variantIndex + 1)
                ++ " variant called. The line looks something like this (x marks the position of the parameter that failed to decode):\n    "
                ++ variantText
                ++ "\n\n"
                |> errorToString_ error

        NoVariantMatches ->
            previousText ++ " in a custom type codec. There wasn't any variants with the correct id. This might mean you've removed a variant and tried to decode data that needed that variant."

        RecordCodecError { fieldIndex, error } ->
            previousText
                ++ " in a record codec, in the "
                ++ Ordinal.ordinal (fieldIndex + 1)
                ++ " field\n\n"
                |> errorToString_ error

        ListCodecError { listIndex, error } ->
            previousText
                ++ " in a list codec, in the "
                ++ Ordinal.ordinal (listIndex + 1)
                ++ " element\n\n"
                |> errorToString_ error

        TupleCodecError { tupleIndex, error } ->
            previousText
                ++ " in a tuple codec, in the "
                ++ Ordinal.ordinal (tupleIndex + 1)
                ++ " element\n\n"
                |> errorToString_ error

        EnumCodecNegativeIndex ->
            previousText ++ "EnumNegativeIndex"

        EnumCodecValueNotFound ->
            previousText ++ "EnumValueNotFound"

        CharCodecError ->
            previousText ++ "CharError"

        BoolCodecError ->
            previousText ++ "BoolError"

        DictCodecError ->
            previousText

        SetCodecError ->
            previousText

        ArrayCodecError { arrayIndex, error } ->
            previousText

        TripleCodecError { tripleIndex, error } ->
            previousText

        MaybeCodecError ->
            previousText

        ResultCodecError ->
            previousText


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
fromBytes : Codec a -> Bytes.Bytes -> Result Error a
fromBytes codec bytes_ =
    case BD.decode (getDecoder codec) bytes_ of
        Just value ->
            value

        Nothing ->
            Err DataCorrupted


fromString : Codec a -> String -> Result Error a
fromString codec base64 =
    case Base64.toBytes base64 of
        Just bytes_ ->
            fromBytes codec bytes_

        Nothing ->
            Err DataCorrupted



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
getEncoder : Codec a -> a -> Encoder
getEncoder (Codec m) =
    m.encoder


{-| Convert an Elm value into a sequence of bytes.
-}
toBytes : Codec a -> a -> Bytes.Bytes
toBytes codec value =
    getEncoder codec value |> BE.encode


toString : Codec a -> a -> String
toString codec =
    toBytes codec >> Base64.fromBytes >> Maybe.withDefault ""



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
            |> BD.map
                (\value ->
                    case value of
                        0 ->
                            Ok False

                        1 ->
                            Ok True

                        _ ->
                            Err BoolCodecError
                )
        )


{-| `Codec` between a signed 32-bit integer and an Elm `Int`
-}
int : Codec Int
int =
    build (toFloat >> BE.float64 endian) (BD.float64 endian |> BD.map (round >> Ok))


{-| `Codec` between a 64-bit float and an Elm `Float`
-}
float : Codec Float
float =
    build (BE.float64 endian) (BD.float64 endian |> BD.map Ok)


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
                            Err CharCodecError
                )
        )



-- DATA STRUCTURES


{-| Represents an optional value.
-}
maybe : Codec a -> Codec (Maybe a)
maybe codec =
    customType
        (\nothingEncoder justEncoder value ->
            case value of
                Nothing ->
                    nothingEncoder

                Just value_ ->
                    justEncoder value_
        )
        |> variant0 Nothing
        |> variant1 Just codec
        |> finishCustomType
        |> map_
            (\value ->
                case value of
                    Ok ok ->
                        Ok ok

                    Err _ ->
                        Err MaybeCodecError
            )
            identity


{-| `Codec` between a sequence of bytes and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list codec =
    Codec
        { encoder = listEncode (getEncoder codec)
        , decoder =
            BD.unsignedInt32 endian
                |> BD.andThen
                    (\length -> BD.loop ( length, [] ) (listStep length (getDecoder codec)))
        }


listEncode : (a -> Encoder) -> List a -> Encoder
listEncode encoder_ list_ =
    list_
        |> List.map encoder_
        |> (::) (BE.unsignedInt32 endian (List.length list_))
        |> BE.sequence


listStep : Int -> BD.Decoder (Result Error a) -> ( Int, List a ) -> Decoder (BD.Step ( Int, List a ) (Result Error (List a)))
listStep length decoder_ ( n, xs ) =
    if n <= 0 then
        BD.succeed (BD.Done (xs |> List.reverse |> Ok))

    else
        BD.map
            (\x ->
                case x of
                    Ok ok ->
                        BD.Loop ( n - 1, ok :: xs )

                    Err err ->
                        BD.Done (ListCodecError { listIndex = length - n, error = err } |> Err)
            )
            decoder_


{-| `Codec` between a sequence of bytes and an Elm `Array`.
-}
array : Codec a -> Codec (Array a)
array codec =
    list codec
        |> map_
            (\value ->
                case value of
                    Ok ok ->
                        Array.fromList ok |> Ok

                    Err (ListCodecError { listIndex, error }) ->
                        ArrayCodecError { arrayIndex = listIndex, error = error } |> Err

                    Err _ ->
                        -- This should never happen.
                        Err DataCorrupted
            )
            Array.toList


{-| `Codec` between a sequence of bytes and an Elm `Dict`.
-}
dict : Codec comparable -> Codec a -> Codec (Dict comparable a)
dict keyCodec valueCodec =
    list (tuple keyCodec valueCodec)
        |> map_
            (\value ->
                case value of
                    Ok ok ->
                        Dict.fromList ok |> Ok

                    Err _ ->
                        Err SetCodecError
            )
            Dict.toList


{-| `Codec` between a sequence of bytes and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set comparable)
set codec =
    list codec
        |> map_
            (\value ->
                case value of
                    Ok ok ->
                        Set.fromList ok |> Ok

                    Err _ ->
                        Err SetCodecError
            )
            Set.toList


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

                        ( Err error, _ ) ->
                            TupleCodecError { tupleIndex = 0, error = error } |> Err

                        ( _, Err error ) ->
                            TupleCodecError { tupleIndex = 1, error = error } |> Err
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

                        ( Err error, _, _ ) ->
                            TripleCodecError { tripleIndex = 0, error = error } |> Err

                        ( _, Err error, _ ) ->
                            TripleCodecError { tripleIndex = 1, error = error } |> Err

                        ( _, _, Err error ) ->
                            TripleCodecError { tripleIndex = 2, error = error } |> Err
                )
                (getDecoder m1)
                (getDecoder m2)
                (getDecoder m3)
        }


{-| `Codec` for `Result` values.
-}
result : Codec error -> Codec value -> Codec (Result error value)
result errorCodec valueCodec =
    customType
        (\errEncoder okEncoder value ->
            case value of
                Err err ->
                    errEncoder err

                Ok ok ->
                    okEncoder ok
        )
        |> variant1 Err errorCodec
        |> variant1 Ok valueCodec
        |> finishCustomType
        |> map_
            (\value ->
                case value of
                    Ok ok ->
                        Ok ok

                    Err _ ->
                        Err ResultCodecError
            )
            identity


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


{-| A codec for an item from a list of possible items.
If you try to encode an item that isn't in the list then the first item is defaulted to.

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

-}
enum : a -> List a -> Codec a
enum defaultItem items =
    build
        (\value ->
            items
                |> findIndex ((==) value)
                |> Maybe.withDefault -1
                |> (+) 1
                |> BE.unsignedInt32 endian
        )
        (BD.unsignedInt32 endian
            |> BD.map
                (\index ->
                    if index < 0 then
                        Err EnumCodecNegativeIndex

                    else if index > List.length items then
                        Err EnumCodecValueNotFound

                    else
                        getAt (index - 1) items |> Maybe.withDefault defaultItem |> Ok
                )
        )


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


{-| <https://github.com/elm-community/list-extra/blob/f9faf1cfa1cec24f977313b1b63e2a1064c36eed/src/List/Extra.elm#L620>
-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


{-| <https://github.com/elm-community/list-extra/blob/f9faf1cfa1cec24f977313b1b63e2a1064c36eed/src/List/Extra.elm#L625>
-}
findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list_ =
    case list_ of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs



-- OBJECTS


{-| A partially built `Codec` for an object.
-}
type RecordCodec a b
    = RecordCodec
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
record : b -> RecordCodec a b
record ctor =
    RecordCodec
        { encoder = \_ -> []
        , decoder = BD.succeed (Ok ctor)
        , fieldCount = 0
        }


{-| Specify how to get a value from the object we want to encode and then give a `Codec` for that value.
-}
field : (a -> f) -> Codec f -> RecordCodec a (f -> b) -> RecordCodec a b
field getter codec (RecordCodec ocodec) =
    RecordCodec
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
                            RecordCodecError { fieldIndex = ocodec.fieldCount, error = xError } |> Err
                )
                ocodec.decoder
                (getDecoder codec)
        , fieldCount = ocodec.fieldCount + 1
        }


{-| Create a `Codec` from a fully specified `ObjectCodec`.
-}
finishRecord : RecordCodec a a -> Codec a
finishRecord (RecordCodec om) =
    Codec
        { encoder = om.encoder >> List.reverse >> BE.sequence
        , decoder = om.decoder
        }



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomTypeCodec match v
    = CustomTypeCodec
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
customType : match -> CustomTypeCodec match value
customType match =
    CustomTypeCodec
        { match = match
        , decoder = \_ -> identity
        , idCounter = 0
        }


variant :
    ((List Encoder -> Encoder) -> a)
    -> (Int -> Decoder (Result Error v))
    -> CustomTypeCodec (a -> b) v
    -> CustomTypeCodec b v
variant matchPiece decoderPiece (CustomTypeCodec am) =
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
    CustomTypeCodec
        { match = am.match <| matchPiece enc
        , decoder = decoder_
        , idCounter = am.idCounter + 1
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 : v -> CustomTypeCodec (Encoder -> a) v -> CustomTypeCodec a v
variant0 ctor =
    variant
        (\c -> c [])
        (\_ -> BD.succeed (Ok ctor))


variantError customIndex variantSize variantIndex error =
    CustomTypeCodecError { variantIndex = customIndex, variantSize = variantSize, variantConstructorIndex = variantIndex, error = error } |> Err


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    (a -> v)
    -> Codec a
    -> CustomTypeCodec ((a -> Encoder) -> b) v
    -> CustomTypeCodec b v
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
                            variantError index 1 0 err
                )
                (getDecoder m1)
        )


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    (a -> b -> v)
    -> Codec a
    -> Codec b
    -> CustomTypeCodec ((a -> b -> Encoder) -> c) v
    -> CustomTypeCodec c v
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
                            variantError index 2 0 err

                        ( _, Err err ) ->
                            variantError index 2 1 err
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
    -> CustomTypeCodec ((a -> b -> c -> Encoder) -> partial) v
    -> CustomTypeCodec partial v
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
                            variantError index 3 0 err

                        ( _, Err err, _ ) ->
                            variantError index 3 1 err

                        ( _, _, Err err ) ->
                            variantError index 3 2 err
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
    -> CustomTypeCodec ((a -> b -> c -> d -> Encoder) -> partial) v
    -> CustomTypeCodec partial v
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
                            variantError index 4 0 err

                        T4 _ (Err err) _ _ ->
                            variantError index 4 1 err

                        T4 _ _ (Err err) _ ->
                            variantError index 4 2 err

                        T4 _ _ _ (Err err) ->
                            variantError index 4 3 err
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
    -> CustomTypeCodec ((a -> b -> c -> d -> e -> Encoder) -> partial) v
    -> CustomTypeCodec partial v
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
                            variantError index 5 0 err

                        T5 _ (Err err) _ _ _ ->
                            variantError index 5 1 err

                        T5 _ _ (Err err) _ _ ->
                            variantError index 5 2 err

                        T5 _ _ _ (Err err) _ ->
                            variantError index 5 3 err

                        T5 _ _ _ _ (Err err) ->
                            variantError index 5 4 err
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
    -> CustomTypeCodec ((a -> b -> c -> d -> e -> f -> Encoder) -> partial) v
    -> CustomTypeCodec partial v
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
                            variantError index 6 0 err

                        T6 _ (Err err) _ _ _ _ ->
                            variantError index 6 1 err

                        T6 _ _ (Err err) _ _ _ ->
                            variantError index 6 2 err

                        T6 _ _ _ (Err err) _ _ ->
                            variantError index 6 3 err

                        T6 _ _ _ _ (Err err) _ ->
                            variantError index 6 4 err

                        T6 _ _ _ _ _ (Err err) ->
                            variantError index 6 5 err
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
    -> CustomTypeCodec ((a -> b -> c -> d -> e -> f -> g -> Encoder) -> partial) v
    -> CustomTypeCodec partial v
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
                            variantError index 7 0 err

                        T7 _ (Err err) _ _ _ _ _ ->
                            variantError index 7 1 err

                        T7 _ _ (Err err) _ _ _ _ ->
                            variantError index 7 2 err

                        T7 _ _ _ (Err err) _ _ _ ->
                            variantError index 7 3 err

                        T7 _ _ _ _ (Err err) _ _ ->
                            variantError index 7 4 err

                        T7 _ _ _ _ _ (Err err) _ ->
                            variantError index 7 5 err

                        T7 _ _ _ _ _ _ (Err err) ->
                            variantError index 7 6 err
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
    -> CustomTypeCodec ((a -> b -> c -> d -> e -> f -> g -> h -> Encoder) -> partial) v
    -> CustomTypeCodec partial v
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
                            variantError index 8 0 err

                        T8 _ (Err err) _ _ _ _ _ _ ->
                            variantError index 8 1 err

                        T8 _ _ (Err err) _ _ _ _ _ ->
                            variantError index 8 2 err

                        T8 _ _ _ (Err err) _ _ _ _ ->
                            variantError index 8 3 err

                        T8 _ _ _ _ (Err err) _ _ _ ->
                            variantError index 8 4 err

                        T8 _ _ _ _ _ (Err err) _ _ ->
                            variantError index 8 5 err

                        T8 _ _ _ _ _ _ (Err err) _ ->
                            variantError index 8 6 err

                        T8 _ _ _ _ _ _ _ (Err err) ->
                            variantError index 8 7 err
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
finishCustomType : CustomTypeCodec (a -> Encoder) a -> Codec a
finishCustomType (CustomTypeCodec am) =
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
map fromBytes_ toBytes_ codec =
    map_
        (\value ->
            case value of
                Ok ok ->
                    fromBytes_ ok |> Ok

                Err err ->
                    Err err
        )
        toBytes_
        codec


map_ : (Result Error a -> Result Error b) -> (b -> a) -> Codec a -> Codec b
map_ fromBytes_ toBytes_ codec =
    Codec
        { decoder = getDecoder codec |> BD.map fromBytes_
        , encoder = \v -> toBytes_ v |> getEncoder codec
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
andThen fromBytes_ toBytes_ codec =
    Codec
        { decoder =
            getDecoder codec
                |> BD.map
                    (\value ->
                        case value of
                            Ok ok ->
                                fromBytes_ ok |> Result.mapError AndThenCodecError

                            Err err ->
                                Err err
                    )
        , encoder = \v -> toBytes_ v |> getEncoder codec
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
