module Codec exposing
    ( Codec
    , Decoder, decoder, decodeValue
    , encoder, encodeToValue
    , string, bool, int, float
    , maybe, list
    , ObjectCodec, object, field, buildObject
    , CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, buildCustom
    , map
    , constant
    --, oneOf
    --, recursive
    --, char
    --, maybe, array, dict, set, tuple, triple, result
    --, variant6, variant7, variant8
    )

{-| A `Codec a` contain a JSON `Decoder a` and the corresponding `a -> Value` encoder.


# Definition

@docs Codec, Value, Error


# Decode

@docs Decoder, decoder, decodeString, decodeValue


# Encode

@docs encoder, encodeToString, encodeToValue


# Primitives

@docs string, bool, int, float, char


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result


# Object Primitives

@docs ObjectCodec, object, field, optionalField, buildObject


# Custom Types

@docs CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom


# Inconsistent structure

@docs oneOf


# Mapping

@docs map


# Fancy Codecs

@docs constant, recursive

-}

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness)
import Bytes.Decode as JD
import Bytes.Encode as JE
import Dict exposing (Dict)
import Set exposing (Set)



-- DEFINITION


{-| A value that knows how to encode and decode JSON values.
-}
type Codec a
    = Codec
        { encoder : a -> JE.Encoder
        , decoder : Decoder a
        }



-- DECODE


{-| A value that knows how to decode JSON values.
-}
type alias Decoder a =
    JD.Decoder a


endian : Endianness
endian =
    Bytes.BE


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
decoder : Codec a -> Decoder a
decoder (Codec m) =
    m.decoder


{-| Run a `Codec` to decode some JSON `Value`. You can send these JSON values
through ports, so that is probably the main time you would use this function.
-}
decodeValue : Codec a -> Bytes -> Maybe a
decodeValue codec =
    JD.decode (decoder codec)



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a -> a -> JE.Encoder
encoder (Codec m) =
    m.encoder


{-| Convert a value into a Javascript `Value`.
-}
encodeToValue : Codec a -> a -> Bytes
encodeToValue codec value =
    encoder codec value |> JE.encode



-- BASE


base : (a -> JE.Encoder) -> Decoder a -> Codec a
base encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        }


{-| `Codec` between a JSON string and an Elm `String`
-}
string : Codec String
string =
    base
        (\text ->
            JE.sequence
                [ JE.unsignedInt32 endian (String.length text)
                , JE.string text
                ]
        )
        (JD.unsignedInt32 endian |> JD.andThen (\charCount -> JD.string charCount))


{-| `Codec` between a JSON boolean and an Elm `Bool`
-}
bool : Codec Bool
bool =
    base
        (\value ->
            if value then
                JE.unsignedInt8 1

            else
                JE.unsignedInt8 0
        )
        (JD.unsignedInt8
            |> JD.andThen
                (\value ->
                    case value of
                        0 ->
                            JD.succeed False

                        1 ->
                            JD.succeed True

                        _ ->
                            JD.fail
                )
        )


{-| `Codec` between a JSON number and an Elm `Int`
-}
int : Codec Int
int =
    base (JE.signedInt32 endian) (JD.signedInt32 endian)


{-| `Codec` between a JSON number and an Elm `Float`
-}
float : Codec Float
float =
    base (JE.float64 endian) (JD.float64 endian)


{-| `Codec` between a JSON string of length 1 and an Elm `Char`
-}



--char : Codec Char
--char =
--    string |> map String.fromChar ()
-- DATA STRUCTURES


build : ((b -> JE.Encoder) -> (a -> JE.Encoder)) -> (Decoder b -> Decoder a) -> Codec b -> Codec a
build enc dec (Codec codec) =
    Codec
        { encoder = enc codec.encoder
        , decoder = dec codec.decoder
        }


{-| Represents an optional value.
-}
maybe : Codec a -> Codec (Maybe a)
maybe codec =
    Codec
        { decoder =
            JD.unsignedInt8
                |> JD.andThen
                    (\value ->
                        case value of
                            0 ->
                                JD.succeed Nothing

                            1 ->
                                decoder codec |> JD.map Just

                            _ ->
                                JD.fail
                    )
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        JE.unsignedInt8 0

                    Just x ->
                        JE.sequence
                            [ JE.unsignedInt8 1
                            , encoder codec x
                            ]
        }


{-| `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list codec =
    --build JE.list JD.list
    Codec
        { encoder = listEncode (encoder codec)
        , decoder =
            JD.unsignedInt32 endian
                |> JD.andThen
                    (\length -> JD.loop ( length, [] ) (listStep (decoder codec)))
        }


listEncode : (a -> JE.Encoder) -> List a -> JE.Encoder
listEncode encoder_ list_ =
    list_
        |> List.map encoder_
        |> List.reverse
        |> (::) (JE.unsignedInt32 endian (List.length list_))
        |> JE.sequence


listStep : JD.Decoder a -> ( Int, List a ) -> Decoder (JD.Step ( Int, List a ) (List a))
listStep decoder_ ( n, xs ) =
    if n <= 0 then
        JD.succeed (JD.Done xs)

    else
        JD.map (\x -> JD.Loop ( n - 1, x :: xs )) decoder_



--{-| `Codec` between a JSON array and an Elm `Array`.
---}
--array : Codec a -> Codec (Array a)
--array =
--    build JE.array JD.array
--
--
--{-| `Codec` between a JSON object and an Elm `Dict`.
---}
--dict : Codec a -> Codec (Dict String a)
--dict =
--    build
--        (\e -> JE.object << Dict.toList << Dict.map (\_ -> e))
--        JD.dict
--
--
--{-| `Codec` between a JSON array and an Elm `Set`.
---}
--set : Codec comparable -> Codec (Set comparable)
--set =
--    build
--        (\e -> JE.list e << Set.toList)
--        (JD.map Set.fromList << JD.list)
--
--
--{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
---}
--tuple : Codec a -> Codec b -> Codec ( a, b )
--tuple m1 m2 =
--    Codec
--        { encoder =
--            \( v1, v2 ) ->
--                JE.list identity
--                    [ encoder m1 v1
--                    , encoder m2 v2
--                    ]
--        , decoder =
--            JD.map2
--                (\a b -> ( a, b ))
--                (JD.index 0 <| decoder m1)
--                (JD.index 1 <| decoder m2)
--        }
--
--
--{-| `Codec` between a JSON array of length 3 and an Elm triple.
---}
--triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
--triple m1 m2 m3 =
--    Codec
--        { encoder =
--            \( v1, v2, v3 ) ->
--                JE.list identity
--                    [ encoder m1 v1
--                    , encoder m2 v2
--                    , encoder m3 v3
--                    ]
--        , decoder =
--            JD.map3
--                (\a b c -> ( a, b, c ))
--                (JD.index 0 <| decoder m1)
--                (JD.index 1 <| decoder m2)
--                (JD.index 2 <| decoder m3)
--        }
--{-| `Codec` for `Result` values.
---}
--result : Codec error -> Codec value -> Codec (Result error value)
--result errorCodec valueCodec =
--    custom
--        (\ferr fok value ->
--            case value of
--                Err err ->
--                    ferr err
--
--                Ok ok ->
--                    fok ok
--        )
--        |> variant1 "Err" Err errorCodec
--        |> variant1 "Ok" Ok valueCodec
--        |> buildCustom
-- OBJECTS


{-| A partially built `Codec` for an object.
-}
type ObjectCodec a b
    = ObjectCodec
        { encoder : a -> List JE.Encoder
        , decoder : Decoder b
        }


{-| Start creating a `Codec` for an object. You should pass the main constructor as argument.
If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an object.
-}
object : b -> ObjectCodec a b
object ctor =
    ObjectCodec
        { encoder = \_ -> []
        , decoder = JD.succeed ctor
        }


{-| Specify the name getter and `Codec` for a field.
-}
field : (a -> f) -> Codec f -> ObjectCodec a (f -> b) -> ObjectCodec a b
field getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder = \v -> (encoder codec <| getter v) :: ocodec.encoder v
        , decoder = JD.map2 (\f x -> f x) ocodec.decoder (decoder codec)
        }


{-| Create a `Codec` from a fully specified `ObjectCodec`.
-}
buildObject : ObjectCodec a a -> Codec a
buildObject (ObjectCodec om) =
    Codec
        { encoder = om.encoder >> List.reverse >> JE.sequence
        , decoder = om.decoder
        }



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomCodec match v
    = CustomCodec
        { match : match
        , decoder : Int -> Decoder v -> Decoder v
        }


{-| Starts building a `Codec` for a custom type.
You need to pass a pattern matching function, see the examples and FAQ for details.
-}
custom : match -> CustomCodec match value
custom match =
    CustomCodec
        { match = match
        , decoder = \_ -> identity
        }


variant :
    Int
    -> ((List JE.Encoder -> JE.Encoder) -> a)
    -> Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
variant name matchPiece decoderPiece (CustomCodec am) =
    let
        enc v =
            JE.unsignedInt32 endian name
                :: v
                |> JE.sequence

        decoder_ tag orElse =
            if tag == name then
                decoderPiece

            else
                am.decoder tag orElse
    in
    CustomCodec
        { match = am.match <| matchPiece enc
        , decoder = decoder_
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    Int
    -> v
    -> CustomCodec (JE.Encoder -> a) v
    -> CustomCodec a v
variant0 name ctor =
    variant name
        (\c -> c [])
        (JD.succeed ctor)


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    Int
    -> (a -> v)
    -> Codec a
    -> CustomCodec ((a -> JE.Encoder) -> b) v
    -> CustomCodec b v
variant1 name ctor m1 =
    variant name
        (\c v ->
            c
                [ encoder m1 v
                ]
        )
        (JD.map ctor
            (decoder m1)
        )


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    Int
    -> (a -> b -> v)
    -> Codec a
    -> Codec b
    -> CustomCodec ((a -> b -> JE.Encoder) -> c) v
    -> CustomCodec c v
variant2 name ctor m1 m2 =
    variant name
        (\c v1 v2 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                ]
        )
        (JD.map2 ctor
            (decoder m1)
            (decoder m2)
        )


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    Int
    -> (a -> b -> c -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> CustomCodec ((a -> b -> c -> JE.Encoder) -> partial) v
    -> CustomCodec partial v
variant3 name ctor m1 m2 m3 =
    variant name
        (\c v1 v2 v3 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                ]
        )
        (JD.map3 ctor
            (decoder m1)
            (decoder m2)
            (decoder m3)
        )


{-| Define a variant with 4 parameters for a custom type.
-}
variant4 :
    Int
    -> (a -> b -> c -> d -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> CustomCodec ((a -> b -> c -> d -> JE.Encoder) -> partial) v
    -> CustomCodec partial v
variant4 name ctor m1 m2 m3 m4 =
    variant name
        (\c v1 v2 v3 v4 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                ]
        )
        (JD.map4 ctor
            (decoder m1)
            (decoder m2)
            (decoder m3)
            (decoder m4)
        )


{-| Define a variant with 5 parameters for a custom type.
-}
variant5 :
    Int
    -> (a -> b -> c -> d -> e -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> CustomCodec ((a -> b -> c -> d -> e -> JE.Encoder) -> partial) v
    -> CustomCodec partial v
variant5 name ctor m1 m2 m3 m4 m5 =
    variant name
        (\c v1 v2 v3 v4 v5 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                ]
        )
        (JD.map5 ctor
            (decoder m1)
            (decoder m2)
            (decoder m3)
            (decoder m4)
            (decoder m5)
        )



--{-| Define a variant with 6 parameters for a custom type.
---}
--variant6 :
--    Int
--    -> (a -> b -> c -> d -> e -> f -> v)
--    -> Codec a
--    -> Codec b
--    -> Codec c
--    -> Codec d
--    -> Codec e
--    -> Codec f
--    -> CustomCodec ((a -> b -> c -> d -> e -> f -> JE.Encoder) -> partial) v
--    -> CustomCodec partial v
--variant6 name ctor m1 m2 m3 m4 m5 m6 =
--    variant name
--        (\c v1 v2 v3 v4 v5 v6 ->
--            c
--                [ encoder m1 v1
--                , encoder m2 v2
--                , encoder m3 v3
--                , encoder m4 v4
--                , encoder m5 v5
--                , encoder m6 v6
--                ]
--        )
--        (JD.map6 ctor
--            (decoder m1)
--            (decoder m2)
--            (decoder m3)
--            (decoder m4)
--            (decoder m5)
--            (decoder m6)
--        )
--
--
--{-| Define a variant with 7 parameters for a custom type.
---}
--variant7 :
--    Int
--    -> (a -> b -> c -> d -> e -> f -> g -> v)
--    -> Codec a
--    -> Codec b
--    -> Codec c
--    -> Codec d
--    -> Codec e
--    -> Codec f
--    -> Codec g
--    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> JE.Encoder) -> partial) v
--    -> CustomCodec partial v
--variant7 name ctor m1 m2 m3 m4 m5 m6 m7 =
--    variant name
--        (\c v1 v2 v3 v4 v5 v6 v7 ->
--            c
--                [ encoder m1 v1
--                , encoder m2 v2
--                , encoder m3 v3
--                , encoder m4 v4
--                , encoder m5 v5
--                , encoder m6 v6
--                , encoder m7 v7
--                ]
--        )
--        (JD.map7 ctor
--            (decoder m1)
--            (decoder m2)
--            (decoder m3)
--            (decoder m4)
--            (decoder m5)
--            (decoder m6)
--            (decoder m7)
--        )
--
--
--{-| Define a variant with 8 parameters for a custom type.
---}
--variant8 :
--    Int
--    -> (a -> b -> c -> d -> e -> f -> g -> h -> v)
--    -> Codec a
--    -> Codec b
--    -> Codec c
--    -> Codec d
--    -> Codec e
--    -> Codec f
--    -> Codec g
--    -> Codec h
--    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> h -> JE.Encoder) -> partial) v
--    -> CustomCodec partial v
--variant8 name ctor m1 m2 m3 m4 m5 m6 m7 m8 =
--    variant name
--        (\c v1 v2 v3 v4 v5 v6 v7 v8 ->
--            c
--                [ encoder m1 v1
--                , encoder m2 v2
--                , encoder m3 v3
--                , encoder m4 v4
--                , encoder m5 v5
--                , encoder m6 v6
--                , encoder m7 v7
--                , encoder m8 v8
--                ]
--        )
--        (JD.map8 ctor
--            (decoder m1)
--            (decoder m2)
--            (decoder m3)
--            (decoder m4)
--            (decoder m5)
--            (decoder m6)
--            (decoder m7)
--            (decoder m8)
--        )


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : CustomCodec (a -> JE.Encoder) a -> Codec a
buildCustom (CustomCodec am) =
    Codec
        { encoder = \v -> am.match v
        , decoder =
            JD.unsignedInt32 endian
                |> JD.andThen
                    (\tag ->
                        am.decoder tag JD.fail
                    )
        }



-- INCONSISTENT STRUCTURE
--{-| Try a set of decoders (in order).
--The first argument is used for encoding and decoding, the list of other codecs is used as a fallback while decoding.
--
--This is particularly useful for backwards compatibility. You would pass the current codec as the first argument,
--and the old ones (eventually `map`ped) as a fallback list to use while decoding.
--
---}
--oneOf : Codec a -> List (Codec a) -> Codec a
--oneOf main alts =
--    Codec
--        { encoder = encoder main
--        , decoder = JD.oneOf <| decoder main :: List.map decoder alts
--        }
-- MAPPING


{-| Transform a `Codec`.
-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map go back codec =
    Codec
        { decoder = JD.map go <| decoder codec
        , encoder = \v -> back v |> encoder codec
        }



---- FANCY
--
--
--{-| Create a `Codec` for a recursive data structure.
--The argument to the function you need to pass is the fully formed `Codec`.
---}
--recursive : (Codec a -> Codec a) -> Codec a
--recursive f =
--    let
--        step =
--            { decoder = JD.lazy (\_ -> decoder <| recursive f)
--            , encoder = \value -> encoder (recursive f) value
--            }
--    in
--    f <| Codec step


{-| Create a `Codec` that produces null as JSON and always decodes as the same value.
-}
constant : a -> Codec a
constant default_ =
    Codec
        { decoder = JD.succeed default_
        , encoder = \_ -> JE.sequence []
        }
