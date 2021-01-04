## What's different between this and miniBill/elm-codec?
Besides the fact that this package is designed for `elm/bytes` instead of `elm/json` other notable differences are:
* `CustomTypeCodec` variants aren't given names and their order matters
* `RecordCodec` fields aren't given names and their order matters 
* There is no `oneOf`, `optionalField`, `andThen`, `recursive` or `build` functions
* `decodeValue`, `encodeToValue` are renamed to `decodeFromJson`, `encodeToJson` respectively (an equivalent to `encoder`, `decoder` is not exposed)

## How do I build `Codec`s for records?

Similar to how it's done in [`NoRedInk/elm-json-decode-pipeline`](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/). 
An example is probably the best way to explain it

```elm
import Serialize as S

type alias Point =
    { x : Int
    , y : Int
    }

pointCodec : S.Codec e Point
pointCodec =
    S.record Point
        -- Note that adding, removing, or reordering fields will prevent you from decoding any data you've previously encoded.
        |> S.field .x S.int
        |> S.field .y S.int
        |> S.finishRecord
```

The first parameter for `Serialize.record` is our constructor. 
Each parameter in our constructor (in this case it's `Point`) is given a 
`Serialize.field` that describes how we get a value from point and how we 
encode/decode that value.

Lastly you end with a call to `finishRecord`.

## How do I build `Codec`s for custom types?
You start building with `customType` which needs the pattern matcher for your type as an argument.

The pattern matcher is just the most generic `case ... of` possible for your type.

You then chain `variantX` calls for every alternative (in the same order as the pattern matcher).

You end with a call to `finishCustomType`.

An example:

```elm
import Serialize as S

type Semaphore
    = Red Int String Bool
    | Yellow Float
    | Green

semaphoreCodec : S.Codec e Semaphore
semaphoreCodec =
    S.customType
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
        |> S.finishCustomType
```

## If there's no `oneOf` or `optionalField`, how is versioning done?
If you want your `Codec`s to evolve over time and still be able to decode old 
encoded data, the recommended approach is to treat the different versions as a custom type.
Then you can write a `customType` `Codec` for those possible versions and use `map` to convert it to the data structure used within your app.

An example:
```elm
import Serialize as S

{-| The gps coordinate we use internally in our application
-}
type alias GpsCoordinate =
    ( Float, Float )

type GpsCoordinateVersions
    = V1GpsCoordinate String -- Old naive way of storing GPS coordinates
    | V2GpsCoordinate GpsCoordinate -- New better way

gpsCoordinateV1Codec =
    S.string

gpsCoordinateV2Codec =
    S.tuple S.float S.float

gpsCoordinateCodec : S.Codec e GpsCoordinate
gpsCoordinateCodec =
    S.customType
        (\gpsCoordinateV1Encoder gpsCoordinateV2Encoder value ->
            case value of
                V1GpsCoordinate text ->
                    gpsCoordinateV1Encoder text

                V2GpsCoordinate tuple ->
                    gpsCoordinateV2Encoder tuple
        )
        |> S.variant1 V1GpsCoordinate v1GpsCoordinateCodec
        |> S.variant1 V2GpsCoordinate v2GpsCoordinateCodec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V1GpsCoordinate text ->
                        v1GpsToGpsCoordinate text

                    V2GpsCoordinate tuple ->
                        tuple -- No conversion needed here
            )
            V2GpsCoordinate

v1GpsToGpsCoordinate : String -> GpsCoordinate
v1GpsToGpsCoordinate =
    Debug.todo "Add the conversion code"
```

