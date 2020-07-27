module Serialize exposing
    ( Codec, Error(..)
    , decodeFromBytes, decodeFromString
    , encodeToBytes, encodeToString
    , string, bool, float, int, bytes, byte
    , maybe, list, array, dict, set, tuple, triple, result, enum
    , RecordCodec, record, field, finishRecord
    , CustomTypeCodec, customType, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, finishCustomType
    , map, mapValid, mapError
    , lazy
    )

{-|


# Definition

@docs Codec, Error


# Decode

@docs decodeFromBytes, decodeFromString


# Encode

@docs encodeToBytes, encodeToString


# Primitives

@docs string, bool, char, float, int, bytes, byte


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result, enum


# Records

@docs RecordCodec, record, field, finishRecord


# Custom Types

@docs CustomTypeCodec, customType, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, finishCustomType


# Mapping

@docs map, mapValid, mapError


# Stack unsafe

@docs lazy

-}

import Array exposing (Array)
import Base64
import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Regex exposing (Regex)
import Set exposing (Set)
import Toop exposing (T4(..), T5(..), T6(..), T7(..), T8(..))



-- DEFINITION


{-| A value that knows how to encode and decode an Elm data structure.
-}
type Codec e a
    = Codec
        { encoder : a -> Encoder
        , decoder : Decoder (Result (Error e) a)
        }


{-| Possible errors that can occur when decoding.

  - `CustomError` - An error caused by `andThen` returning an Err value.
  - `DataCorrupted` - This most likely will occur if you made breaking changes to your codec and try to decode old data. Have a look at `How do I change my data structures and still be able to decode data I've previously encoded?` in the readme for how to avoid introducing breaking changes.
  - `SerializerOutOfDate` - When encoding, this package will include a version number. This makes it possible for me to make improvements to how data gets encoded without introducing breaking changes to your codecs. This error then, says that you're trying to decode data encoded with a newer version of elm-serialize.

-}
type Error e
    = CustomError e
    | DataCorrupted
    | SerializerOutOfDate


version : Int
version =
    1


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
getDecoder : Codec e a -> Decoder (Result (Error e) a)
getDecoder (Codec m) =
    m.decoder


{-| Run a `Codec` to turn a sequence of bytes into an Elm value.
-}
decodeFromBytes : Codec e a -> Bytes.Bytes -> Result (Error e) a
decodeFromBytes codec bytes_ =
    let
        decoder =
            BD.unsignedInt8
                |> BD.andThen
                    (\value ->
                        if value <= 0 then
                            Err DataCorrupted |> BD.succeed

                        else if value == version then
                            getDecoder codec

                        else
                            Err SerializerOutOfDate |> BD.succeed
                    )
    in
    case BD.decode decoder bytes_ of
        Just value ->
            value

        Nothing ->
            Err DataCorrupted


{-| Run a `Codec` to turn a String encoded with `encodeToString` into an Elm value.
-}
decodeFromString : Codec e a -> String -> Result (Error e) a
decodeFromString codec base64 =
    case decode base64 of
        Just bytes_ ->
            decodeFromBytes codec bytes_

        Nothing ->
            Err DataCorrupted


decode : String -> Maybe Bytes.Bytes
decode base64text =
    let
        replaceChar rematch =
            case rematch.match of
                "-" ->
                    "+"

                _ ->
                    "/"

        strlen =
            String.length base64text
    in
    if strlen == 0 then
        BE.encode (BE.sequence []) |> Just

    else
        let
            hanging =
                modBy 4 strlen

            ilen =
                if hanging == 0 then
                    0

                else
                    4 - hanging
        in
        Regex.replace replaceFromUrl replaceChar (base64text ++ String.repeat ilen "=") |> Base64.toBytes


replaceFromUrl : Regex
replaceFromUrl =
    Regex.fromString "[-_]" |> Maybe.withDefault Regex.never



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
getEncoder : Codec e a -> a -> Encoder
getEncoder (Codec m) =
    m.encoder


{-| Convert an Elm value into a sequence of bytes.
-}
encodeToBytes : Codec e a -> a -> Bytes.Bytes
encodeToBytes codec value =
    BE.sequence
        [ BE.unsignedInt8 version
        , value |> getEncoder codec
        ]
        |> BE.encode


{-| Convert an Elm value into a string. This string contains only url safe characters, so you can do the following:

    import Serlialize as S

    myUrl =
        "www.mywebsite.com/?data=" ++ S.encodeToString S.float 1234

and not risk generating an invalid url.

-}
encodeToString : Codec e a -> a -> String
encodeToString codec =
    encodeToBytes codec >> replaceBase64Chars


replaceBase64Chars : Bytes.Bytes -> String
replaceBase64Chars =
    let
        replaceChar rematch =
            case rematch.match of
                "+" ->
                    "-"

                "/" ->
                    "_"

                _ ->
                    ""
    in
    Base64.fromBytes >> Maybe.withDefault "" >> Regex.replace replaceForUrl replaceChar


replaceForUrl : Regex
replaceForUrl =
    Regex.fromString "[\\+/=]" |> Maybe.withDefault Regex.never



-- BASE


build : (a -> Encoder) -> Decoder (Result (Error e) a) -> Codec e a
build encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        }


{-| Codec for serializing a `String`
-}
string : Codec e String
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


{-| Codec for serializing a `Bool`
-}
bool : Codec e Bool
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
                            Err DataCorrupted
                )
        )


{-| Codec for serializing an `Int`
-}
int : Codec e Int
int =
    build (toFloat >> BE.float64 endian) (BD.float64 endian |> BD.map (round >> Ok))


{-| Codec for serializing a `Float`
-}
float : Codec e Float
float =
    build (BE.float64 endian) (BD.float64 endian |> BD.map Ok)


{-| Codec for serializing a `Char`
-}
char : Codec e Char
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
                            Err DataCorrupted
                )
        )



-- DATA STRUCTURES


{-| Codec for serializing a `Maybe`

    import Serialize as S

    maybeIntCodec : S.Codec (Maybe Int)
    maybeIntCodec =
        S.Maybe S.Int

-}
maybe : Codec e a -> Codec e (Maybe a)
maybe justCodec =
    customType
        (\nothingEncoder justEncoder value ->
            case value of
                Nothing ->
                    nothingEncoder

                Just value_ ->
                    justEncoder value_
        )
        |> variant0 Nothing
        |> variant1 Just justCodec
        |> finishCustomType


{-| Codec for serializing a `List`

    import Serialize as S

    listOfStringsCodec : S.Codec (List String)
    listOfStringsCodec =
        S.list S.string

-}
list : Codec e a -> Codec e (List a)
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
        |> (::) (BE.unsignedInt32 endian (List.length list_))
        |> BE.sequence


listStep : BD.Decoder (Result (Error e) a) -> ( Int, List a ) -> Decoder (BD.Step ( Int, List a ) (Result (Error e) (List a)))
listStep decoder_ ( n, xs ) =
    if n <= 0 then
        BD.succeed (BD.Done (xs |> List.reverse |> Ok))

    else
        BD.map
            (\x ->
                case x of
                    Ok ok ->
                        BD.Loop ( n - 1, ok :: xs )

                    Err err ->
                        BD.Done (Err err)
            )
            decoder_


{-| Codec for serializing an `Array`
-}
array : Codec e a -> Codec e (Array a)
array codec =
    list codec |> map_ (Result.map Array.fromList) Array.toList


{-| Codec for serializing a `Dict`

    import Serialize as S

    type alias Name =
        String

    peoplesAgeCodec : S.Codec (Dict Name Int)
    peoplesAgeCodec =
        S.dict S.string S.int

-}
dict : Codec e comparable -> Codec e a -> Codec e (Dict comparable a)
dict keyCodec valueCodec =
    list (tuple keyCodec valueCodec)
        |> map_ (Result.map Dict.fromList) Dict.toList


{-| Codec for serializing a `Set`
-}
set : Codec e comparable -> Codec e (Set comparable)
set codec =
    list codec |> map_ (Result.map Set.fromList) Set.toList


{-| Codec for serializing a tuple with 2 elements

    import Serialize as S

    pointCodec : S.Codec ( Float, Float )
    pointCodec =
        S.tuple S.float S.float

-}
tuple : Codec e a -> Codec e b -> Codec e ( a, b )
tuple codecFirst codecSecond =
    record Tuple.pair
        |> field Tuple.first codecFirst
        |> field Tuple.second codecSecond
        |> finishRecord


{-| Codec for serializing a tuple with 3 elements

    import Serialize as S

    pointCodec : S.Codec ( Float, Float, Float )
    pointCodec =
        S.tuple S.float S.float S.float

-}
triple : Codec e a -> Codec e b -> Codec e c -> Codec e ( a, b, c )
triple codecFirst codecSecond codecThird =
    record (\a b c -> ( a, b, c ))
        |> field (\( a, _, _ ) -> a) codecFirst
        |> field (\( _, b, _ ) -> b) codecSecond
        |> field (\( _, _, c ) -> c) codecThird
        |> finishRecord


{-| Codec for serializing a `Result`
-}
result : Codec e error -> Codec e value -> Codec e (Result error value)
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


{-| Codec for serializing [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/).
This is useful in combination with `mapValid` to encode and decode external binary data formats.

    import Image exposing (Image)
    import Serialize as S

    imageCodec : S.Codec Image
    imageCodec =
        S.bytes
            |> S.mapValid
                (Image.decode >> Result.fromMaybe (AndThenError { errorMessage = "Invalid png" }))
                Image.toPng

-}
bytes : Codec e Bytes.Bytes
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


{-| Codec for serializing an integer ranging from 0 to 255.
This is useful if you have a small integer you want to serialize and not use up a lot of space.

    import Serialize as S

    type alias Color =
        { red : Int
        , green : Int
        , blue : Int
        }

    color : S.Codec Color
    color =
        Color.record Color
            |> S.field .red byte
            |> S.field .green byte
            |> S.field .blue byte
            |> S.finishRecord

-}
byte : Codec e Int
byte =
    Codec
        { encoder = BE.unsignedInt8
        , decoder = BD.unsignedInt8 |> BD.map Ok
        }


{-| A codec for serializing an item from a list of possible items.
If you try to encode an item that isn't in the list then the first item is defaulted to.

    import Serialize as S

    type DaysOfWeek
        = Monday
        | Tuesday
        | Wednesday
        | Thursday
        | Friday
        | Saturday
        | Sunday

    daysOfWeekCodec : S.Codec DaysOfWeek
    daysOfWeekCodec =
        S.enum Monday [ Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]

-}
enum : a -> List a -> Codec e a
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
                        Err DataCorrupted

                    else if index > List.length items then
                        Err DataCorrupted

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


{-| A partially built Codec for a record.
-}
type RecordCodec e a b
    = RecordCodec
        { encoder : a -> List Encoder
        , decoder : Decoder (Result (Error e) b)
        , fieldCount : Int
        }


{-| Start creating a codec for a record.

    import Serialize as S

    type alias Point =
        { x : Int
        , y : Int
        }

    pointCodec : S.Codec Point
    pointCodec =
        S.object Point
            -- Note that adding, removing, or reordering fields will prevent you from decoding any data you've previously encoded.
            |> S.field .x S.int
            |> S.field .y S.int
            |> S.finishObject

-}
record : b -> RecordCodec e a b
record ctor =
    RecordCodec
        { encoder = \_ -> []
        , decoder = BD.succeed (Ok ctor)
        , fieldCount = 0
        }


{-| Add a field to the record we are creating a codec for.
-}
field : (a -> f) -> Codec e f -> RecordCodec e a (f -> b) -> RecordCodec e a b
field getter codec (RecordCodec recordCodec) =
    RecordCodec
        { encoder = \v -> (getEncoder codec <| getter v) :: recordCodec.encoder v
        , decoder =
            BD.map2
                (\f x ->
                    case ( f, x ) of
                        ( Ok fOk, Ok xOk ) ->
                            fOk xOk |> Ok

                        ( Err err, _ ) ->
                            Err err

                        ( _, Err err ) ->
                            Err err
                )
                recordCodec.decoder
                (getDecoder codec)
        , fieldCount = recordCodec.fieldCount + 1
        }


{-| Finish creating a codec for a record.
-}
finishRecord : RecordCodec e a a -> Codec e a
finishRecord (RecordCodec codec) =
    Codec
        { encoder = codec.encoder >> List.reverse >> BE.sequence
        , decoder = codec.decoder
        }



-- CUSTOM


{-| A partially built codec for a custom type.
-}
type CustomTypeCodec a e match v
    = CustomTypeCodec
        { match : match
        , decoder : Int -> Decoder (Result (Error e) v) -> Decoder (Result (Error e) v)
        , idCounter : Int
        }


{-| Starts building a `Codec` for a custom type.
You need to pass a pattern matching function, see the FAQ for details.

    import Serialize as S

    type Semaphore
        = Red Int String Bool
        | Yellow Float
        | Green

    semaphoreCodec : S.Codec Semaphore
    semaphoreCodec =
        S.custom
            (\redEncoder yellowEncoder greenEncoder value ->
                case value of
                    Red i s b ->
                        redEncoder i s b

                    Yellow f ->
                        yellowEncoder f

                    Green ->
                        greenEncoder
            )
            -- Note that removing a variant, inserting a variant before an existing one, or swapping two variants will prevent you from decoding any data you've previously encoded.
            |> S.variant3 Red S.int S.string S.bool
            |> S.variant1 Yellow S.float
            |> S.variant0 Green
            -- It's safe to add new variants here later though
            |> S.finishCustom

-}
customType : match -> CustomTypeCodec youNeedAtLeastOneVariant e match value
customType match =
    CustomTypeCodec
        { match = match
        , decoder = \_ -> identity
        , idCounter = 0
        }


variant :
    ((List Encoder -> Encoder) -> a)
    -> Decoder (Result (Error error) v)
    -> CustomTypeCodec z error (a -> b) v
    -> CustomTypeCodec () error b v
variant matchPiece decoderPiece (CustomTypeCodec am) =
    let
        enc v =
            BE.unsignedInt16 endian am.idCounter
                :: v
                |> BE.sequence

        decoder_ tag orElse =
            if tag == am.idCounter then
                decoderPiece

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
variant0 : v -> CustomTypeCodec z e (Encoder -> a) v -> CustomTypeCodec () e a v
variant0 ctor =
    variant
        (\c -> c [])
        (BD.succeed (Ok ctor))


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    (a -> v)
    -> Codec error a
    -> CustomTypeCodec z error ((a -> Encoder) -> b) v
    -> CustomTypeCodec () error b v
variant1 ctor m1 =
    variant
        (\c v ->
            c
                [ getEncoder m1 v
                ]
        )
        (BD.map
            (\value ->
                case value of
                    Ok ok ->
                        ctor ok |> Ok

                    Err err ->
                        Err err
            )
            (getDecoder m1)
        )


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    (a -> b -> v)
    -> Codec error a
    -> Codec error b
    -> CustomTypeCodec z error ((a -> b -> Encoder) -> c) v
    -> CustomTypeCodec () error c v
variant2 ctor m1 m2 =
    variant
        (\c v1 v2 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                ]
        )
        (BD.map2
            (\v1 v2 ->
                case ( v1, v2 ) of
                    ( Ok ok1, Ok ok2 ) ->
                        ctor ok1 ok2 |> Ok

                    ( Err err, _ ) ->
                        Err err

                    ( _, Err err ) ->
                        Err err
            )
            (getDecoder m1)
            (getDecoder m2)
        )


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    (a -> b -> c -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> CustomTypeCodec z error ((a -> b -> c -> Encoder) -> partial) v
    -> CustomTypeCodec () error partial v
variant3 ctor m1 m2 m3 =
    variant
        (\c v1 v2 v3 ->
            c
                [ getEncoder m1 v1
                , getEncoder m2 v2
                , getEncoder m3 v3
                ]
        )
        (BD.map3
            (\v1 v2 v3 ->
                case ( v1, v2, v3 ) of
                    ( Ok ok1, Ok ok2, Ok ok3 ) ->
                        ctor ok1 ok2 ok3 |> Ok

                    ( Err err, _, _ ) ->
                        Err err

                    ( _, Err err, _ ) ->
                        Err err

                    ( _, _, Err err ) ->
                        Err err
            )
            (getDecoder m1)
            (getDecoder m2)
            (getDecoder m3)
        )


{-| Define a variant with 4 parameters for a custom type.
-}
variant4 :
    (a -> b -> c -> d -> v)
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> CustomTypeCodec z error ((a -> b -> c -> d -> Encoder) -> partial) v
    -> CustomTypeCodec () error partial v
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
        (BD.map4
            (\v1 v2 v3 v4 ->
                case T4 v1 v2 v3 v4 of
                    T4 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) ->
                        ctor ok1 ok2 ok3 ok4 |> Ok

                    T4 (Err err) _ _ _ ->
                        Err err

                    T4 _ (Err err) _ _ ->
                        Err err

                    T4 _ _ (Err err) _ ->
                        Err err

                    T4 _ _ _ (Err err) ->
                        Err err
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
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> Encoder) -> partial) v
    -> CustomTypeCodec () error partial v
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
        (BD.map5
            (\v1 v2 v3 v4 v5 ->
                case T5 v1 v2 v3 v4 v5 of
                    T5 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) ->
                        ctor ok1 ok2 ok3 ok4 ok5 |> Ok

                    T5 (Err err) _ _ _ _ ->
                        Err err

                    T5 _ (Err err) _ _ _ ->
                        Err err

                    T5 _ _ (Err err) _ _ ->
                        Err err

                    T5 _ _ _ (Err err) _ ->
                        Err err

                    T5 _ _ _ _ (Err err) ->
                        Err err
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
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> Codec error f
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> Encoder) -> partial) v
    -> CustomTypeCodec () error partial v
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
        (BD.map5
            (\v1 v2 v3 v4 ( v5, v6 ) ->
                case T6 v1 v2 v3 v4 v5 v6 of
                    T6 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) ->
                        ctor ok1 ok2 ok3 ok4 ok5 ok6 |> Ok

                    T6 (Err err) _ _ _ _ _ ->
                        Err err

                    T6 _ (Err err) _ _ _ _ ->
                        Err err

                    T6 _ _ (Err err) _ _ _ ->
                        Err err

                    T6 _ _ _ (Err err) _ _ ->
                        Err err

                    T6 _ _ _ _ (Err err) _ ->
                        Err err

                    T6 _ _ _ _ _ (Err err) ->
                        Err err
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
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> Codec error f
    -> Codec error g
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> g -> Encoder) -> partial) v
    -> CustomTypeCodec () error partial v
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
        (BD.map5
            (\v1 v2 v3 ( v4, v5 ) ( v6, v7 ) ->
                case T7 v1 v2 v3 v4 v5 v6 v7 of
                    T7 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) (Ok ok7) ->
                        ctor ok1 ok2 ok3 ok4 ok5 ok6 ok7 |> Ok

                    T7 (Err err) _ _ _ _ _ _ ->
                        Err err

                    T7 _ (Err err) _ _ _ _ _ ->
                        Err err

                    T7 _ _ (Err err) _ _ _ _ ->
                        Err err

                    T7 _ _ _ (Err err) _ _ _ ->
                        Err err

                    T7 _ _ _ _ (Err err) _ _ ->
                        Err err

                    T7 _ _ _ _ _ (Err err) _ ->
                        Err err

                    T7 _ _ _ _ _ _ (Err err) ->
                        Err err
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
    -> Codec error a
    -> Codec error b
    -> Codec error c
    -> Codec error d
    -> Codec error e
    -> Codec error f
    -> Codec error g
    -> Codec error h
    -> CustomTypeCodec z error ((a -> b -> c -> d -> e -> f -> g -> h -> Encoder) -> partial) v
    -> CustomTypeCodec () error partial v
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
        (BD.map5
            (\v1 v2 ( v3, v4 ) ( v5, v6 ) ( v7, v8 ) ->
                case T8 v1 v2 v3 v4 v5 v6 v7 v8 of
                    T8 (Ok ok1) (Ok ok2) (Ok ok3) (Ok ok4) (Ok ok5) (Ok ok6) (Ok ok7) (Ok ok8) ->
                        ctor ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 |> Ok

                    T8 (Err err) _ _ _ _ _ _ _ ->
                        Err err

                    T8 _ (Err err) _ _ _ _ _ _ ->
                        Err err

                    T8 _ _ (Err err) _ _ _ _ _ ->
                        Err err

                    T8 _ _ _ (Err err) _ _ _ _ ->
                        Err err

                    T8 _ _ _ _ (Err err) _ _ _ ->
                        Err err

                    T8 _ _ _ _ _ (Err err) _ _ ->
                        Err err

                    T8 _ _ _ _ _ _ (Err err) _ ->
                        Err err

                    T8 _ _ _ _ _ _ _ (Err err) ->
                        Err err
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


{-| Finish creating a codec for a custom type.
-}
finishCustomType : CustomTypeCodec () e (a -> Encoder) a -> Codec e a
finishCustomType (CustomTypeCodec am) =
    Codec
        { encoder = \v -> am.match v
        , decoder =
            BD.unsignedInt16 endian
                |> BD.andThen
                    (\tag ->
                        am.decoder tag (BD.succeed (Err DataCorrupted))
                    )
        }



---- MAPPING


{-| Map from one codec to another codec

    import Serialize as S

    type UserId
        = UserId Int

    userIdCodec : S.Codec UserId
    userIdCodec =
        S.int |> S.map UserId (\(UserId id) -> id)

-}
map : (a -> b) -> (b -> a) -> Codec e a -> Codec e b
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


map_ : (Result (Error e) a -> Result (Error e) b) -> (b -> a) -> Codec e a -> Codec e b
map_ fromBytes_ toBytes_ codec =
    Codec
        { decoder = getDecoder codec |> BD.map fromBytes_
        , encoder = \v -> toBytes_ v |> getEncoder codec
        }


{-| Map from one codec to another codec in a way that can potentially fail when decoding.

    -- Email module is from https://package.elm-lang.org/packages/tricycle/elm-email/1.0.2/


    import Email
    import Serialize as S

    emailCodec : S.Codec String Float
    emailCodec =
        S.string
            |> S.mapValid
                (\text ->
                    case Email.fromString of
                        Just email ->
                            Ok email

                        Nothing ->
                            Err "Invalid email"
                )
                Email.toString

-}
mapValid : (a -> Result e b) -> (b -> a) -> Codec e a -> Codec e b
mapValid fromBytes_ toBytes_ codec =
    Codec
        { decoder =
            getDecoder codec
                |> BD.map
                    (\value ->
                        case value of
                            Ok ok ->
                                fromBytes_ ok |> Result.mapError CustomError

                            Err err ->
                                Err err
                    )
        , encoder = \v -> toBytes_ v |> getEncoder codec
        }


{-| Map errors generated by `mapValid`.
-}
mapError : (e1 -> e2) -> Codec e1 a -> Codec e2 a
mapError mapFunc codec =
    Codec
        { encoder = getEncoder codec
        , decoder =
            getDecoder codec
                |> BD.map
                    (Result.mapError
                        (\error ->
                            case error of
                                CustomError custom ->
                                    mapFunc custom |> CustomError

                                DataCorrupted ->
                                    DataCorrupted

                                SerializerOutOfDate ->
                                    SerializerOutOfDate
                        )
                    )
        }



-- STACK UNSAFE


{-| Handle situations where you need to define a codec in terms of itself.

    import Serialize as S

    type Peano
        = Peano (Maybe Peano)

    {-| The compiler will complain that this function causes an infinite loop.
    -}
    badPeanoCodec : S.Codec Peano
    badPeanoCodec =
        S.maybe badPeanoCodec |> S.map Peano (\(Peano a) -> a)

    {-| Now the compiler is happy!
    -}
    goodPeanoCodec : S.Codec Peano
    goodPeanoCodec =
        S.maybe (S.lazy (\() -> goodPeanoCodec)) |> S.map Peano (\(Peano a) -> a)

**Warning:** `lazy` is _not_ stack safe!
If you have something like `Peano (Just (Peano Just (...)))` nested within itself sufficiently many times and you try to use `peanoCodec` on it, you'll get a stack overflow!

-}
lazy : (() -> Codec e a) -> Codec e a
lazy f =
    Codec
        { decoder = BD.succeed () |> BD.andThen (\() -> getDecoder (f ()))
        , encoder = \value -> getEncoder (f ()) value
        }
