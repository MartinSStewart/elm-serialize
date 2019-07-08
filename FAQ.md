## What's different between this and miniBill/elm-codec?
Besides the fact that this package is designed for `elm/bytes` instead of `elm/json` other notable differences are:
* `variantX` functions use `Int` instead of `String` for telling apart different constructors
* `CustomObject` fields aren't given names but their order matters 
* There is no `oneOf` or `optionalField` in this package
* There are more ways to encode elm `int` and `float` values (i.e. `signedInt`, `unsignedInt`, `float32`, `float64`)

## How do you use `recursive`?
The trick to understanding the `recursive` codec is: pretend you are already done.
When the function you pass to `recursive` is called the argument is the finished `Codec`.

An example may be worth a thousand words:

```elm
type Peano
    = Peano (Maybe Peano)


peanoCodec : Codec Peano
peanoCodec =
    Codec.recursive
        (\finishedCodec ->
            Codec.maybe finishedCodec
                |> Codec.map Peano (\(Peano p) -> p)
        )
```

## Why does `map` take two opposite functions?
One is used for the encoder, the other for the decoder

## How do I build `Codec`s for custom types?
You start building with `custom` which needs the pattern matcher for your type as an argument.

The pattern matcher is just the most generic `case ... of` possible for your type.

You then chain `variantX` calls for every alternative (in the same order as the pattern matcher).

You end with a call to `buildCustom`.

An example:

```elm
type Semaphore
    = Red Int String Bool
    | Yellow Float
    | Green


semaphoreCodec : Codec Semaphore
semaphoreCodec =
    Codec.custom
        (\fred fyellow fgreen value ->
            case value of
                Red i s b ->
                    fred i s b

                Yellow f ->
                    fyellow f

                Green ->
                    fgreen
        )
        |> Codec.variant3 0 Red Codec.signedInt Codec.string Codec.bool
        |> Codec.variant1 1 Yellow Codec.float64
        |> Codec.variant0 2 Green
        |> Codec.buildCustom
```

## If there's no `oneOf` or `optionalField`, how is versioning done?
If you want your `Codec`s to evolve over time and still be able to decode old 
encoded data, the recommended approach is to treat the different versions as a custom type.
Then you can write a `custom` Codec for those possible versions and use `map` to convert it to the data structure used within your app.

An example:
```elm
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
        (\fv1 fv2 value ->
            case value of
                GpsV1 text ->
                    fv1 text

                GpsV2 tuple ->
                    fv2 tuple
        )
        |> Codec.variant1 1 GpsV1 gpsV1Codec
        |> Codec.variant1 2 GpsV2 gpsV2Codec
        |> Codec.buildCustom
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