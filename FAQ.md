## What's different between this and miniBill/elm-codec?
Besides the fact that this package is designed for `elm/bytes` instead of `elm/json` other notable differences are:
* `CustomCodec` variants aren't given names and their order matters
* `ObjectCodec` fields aren't given names and their order matters 
* There is no `oneOf`, `optionalField`, `andThen`, `recursive` or `build` functions
* There are more ways to encode elm `int` and `float` values (i.e. `signedInt32`, `unsignedInt32`, `float32`, `float64`)
* `decodeValue`, `encodeToValue`, `encoder`, and `decoder` are renamed to `decode`, `encode`, `getEncoder`, `getDecoder` respectively

## How do I build `Codec`s for records?

Similar to how it's done in [`NoRedInk/elm-json-decode-pipeline`](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/). 
An example is probably the best way to explain it

```elm
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
```

The first parameter for `Codec.object` is our constructor. 
Each parameter in our constructor (in this case it's `Point`) is given a 
`Codec.field` that describes how we get a value from point and how we 
encode/decode that value.

Lastly you end with a call to `finishObject`.

## How do I build `Codec`s for custom types?
You start building with `custom` which needs the pattern matcher for your type as an argument.

The pattern matcher is just the most generic `case ... of` possible for your type.

You then chain `variantX` calls for every alternative (in the same order as the pattern matcher).

You end with a call to `finishCustom`.

An example:

```elm
import Codec.Bytes as Codec exposing (Codec)

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
```

## If there's no `oneOf` or `optionalField`, how is versioning done?
If you want your `Codec`s to evolve over time and still be able to decode old 
encoded data, the recommended approach is to treat the different versions as a custom type.
Then you can write a `custom` Codec for those possible versions and use `map` to convert it to the data structure used within your app.

An example:
```elm
import Codec.Bytes as Codec exposing (Codec)

{-| The gps coordinate we use internally in our application
-}
type alias GpsCoordinate =
    ( Float, Float )

type GpsVersions
    = GpsV1 String -- Old naive way of storing GPS coordinates
    | GpsV2 ( Float, Float ) -- New better way

gpsV1Codec =
    Codec.string

gpsV2Codec =
    Codec.tuple Codec.float64 Codec.float64

gpsCodec : Codec GpsCoordinate
gpsCodec =
    Codec.custom
        (\gpsV1Encoder gpsV2Encoder value ->
            case value of
                GpsV1 text ->
                    gpsV1Encoder text

                GpsV2 tuple ->
                    gpsV2Encoder tuple
        )
        |> Codec.variant1 GpsV1 gpsV1Codec
        |> Codec.variant1 GpsV2 gpsV2Codec
        |> Codec.finishCustom
        |> Codec.map
            (\value ->
                case value of
                    GpsV1 text ->
                        convertGpsV1ToGpsCoordinate text

                    GpsV2 tuple ->
                        tuple -- No conversion needed here
            )
            (\value -> GpsV2 value)
```

