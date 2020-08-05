# elm-serialize

Quickly and reliably write code to handle serialization of Elm data structures. 
This is done via `Codec`s which automatically create both the encoder and decoder ensuring that they don't get out of sync with eachother.

### What elm-serialize is good for?
- Sparing you from having to write both encoders and decoders
- Reliably encoding and decoding Elm types (no more failing to decode because you made a typo somewhere!)
- The data format is an implementation detail so you can use `encodeToJson`, `encodeToBytes`, or `encodeToString`.

### What elm-serialize is *not* good for?
- Decoding external data formats
- Encoding to a human readable format

## Basic usage

```elm
import Serialize as S

type alias MeAndMyFriends = 
    { me : String
    , myFriends : List String
    }

friendsCodec : S.Codec e (List String)
friendsCodec =
    S.list S.string
    
meAndMyFriendsCodec : S.Codec e MeAndMyFriends
meAndMyFriendsCodec =
    S.record Model 
        |> S.field .me S.string
        |> S.field .myFriends friendsCodec
        |> S.finishRecord

encode : MeAndMyFriends -> Bytes
encode meAndMyFriends =
    S.encodeToBytes meAndMyFriendsCodec meAndMyFriends

decode : Bytes -> Result (S.Error e) MeAndMyFriends
decode data =
    S.decodeFromBytes meAndMyFriendsCodec data
```

## Writing codecs for custom types

```elm
import Serialize as S

type Semaphore
    = Red Int String Bool
    | Yellow Float
    | Green

semaphoreCodec : S.Codec Semaphore
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
        |> S.variant3 Red S.int S.string S.bool
        |> S.variant1 Yellow S.float
        |> S.variant0 Green
        |> S.finishCustomType
```

## Why isn't there a codec for `Char`?

`Char` has problems https://github.com/elm/core/issues/1001

There are a variety of unicode code points that get transformed into multiple code points when you change their casing, but Elm still treats these as `Char`.
So if we made a codec for this we'd drop the extra code points and end up with something like this:
```elm
import Serialize as S

Char.toUpper 'ß' --> 'SS'
    |> S.encodeToBytes hypotheticalCharCodec
    |> S.decodeFromBytes --> Ok 'S'
    
```

So the short of it is, it's not possible to create a `Char` that won't sometimes mess up your data when decoding.

## How do I change my codecs and still be able to decode old data?

First let's cover what counts as a breaking change for a Codec.

For records, adding or removing `field` is a breaking change.
Changing the Codec used in a `field` is also a breaking change.

For custom types, removing a `variant` is a breaking change.
Changing one of the Codecs used in a `variant` or changing how many Codecs are used is a breaking change.

Appending a `variant` is *not* a breaking change.
```elm
import Serialize as S

-- We've enhanced the semaphore with a rainbow variant!
type Semaphore
    = Red Int String Bool
    | Yellow Float
    | Green
    | Rainbow 

S.custom
    (\redEncoder yellowEncoder greenEncoder rainbowEncoder value ->
        case value of
            Red i s b ->
                redEncoder i s b

            Yellow f ->
                yellowEncoder f

            Green ->
                greenEncoder
                
            Rainbow ->
                rainbowEncoder
    )
    |> S.variant3 Red S.int S.string S.bool
    |> S.variant1 Yellow S.float
    |> S.variant0 Green
    -- We can safely add the new variant here at the end.
    |> S.variant0 Rainbow
    |> S.finishCustom
```
The example above will still decode anything encoded with the semaphoreCodec in the [custom types example](#writing-codecs-for-custom-types)

Knowing this, what we can do is have a top level custom type that lets us handle different versions of our Codec.

Suppose we are making an app that can serialize the users GPS coordinate.
We are in a rush to get this app working so we just store GPS coordinates as a string.

Here's an example of what that would look like:
```elm
import Serialize as S

{-| The gps coordinate used internally in our application
-}
type alias GpsCoordinate =
    String

type GpsVersions
    = GpsV1 GpsCoordinate

gpsV1Codec : GpsCoordinate
gpsV1Codec =
    S.string

gpsCodec : S.Codec GpsCoordinate
gpsCodec =
    S.custom
        (\gpsV1Encoder value ->
            case value of
                GpsV1 text ->
                    gpsV1Encoder text
        )
        |> S.variant1 GpsV1 gpsV1Codec
        |> S.finishCustom
        |> S.map
            (\value ->
                case value of
                    GpsV1 text ->
                        text
            )
            (\value -> GpsV1 value)
```

Then a while later we start refactoring the app. Internally we replace all those GPS strings with `(Float, Float)`.
We still want to decode all the serialized data though so we change our module to look like this:

```elm
import Serialize as S

{-| The gps coordinate used internally in our application
-}
type alias GpsCoordinate =
    ( Float, Float )


type GpsVersions
    = GpsV1 String -- Old way of storing GPS coordinates
    | GpsV2 GpsCoordinate -- New better way

gpsV1Codec : S.Codec e String
gpsV1Codec =
    S.string

gpsV2Codec : S.Codec e GpsCoordinate
gpsV2Codec =
    S.tuple S.float S.float

gpsCodec : S.Codec e GpsCoordinate
gpsCodec =
    S.customType
        (\gpsV1Encoder gpsV2Encoder value ->
            case value of
                GpsV1 text ->
                    gpsV1Encoder text

                GpsV2 tuple ->
                    gpsV2Encoder tuple
        )
        |> S.variant1 GpsV1 gpsV1Codec
        -- We append our new GPS codec. This is not a breaking change.
        |> S.variant1 GpsV2 gpsV2Codec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    GpsV1 text ->
                        -- After we've decoded an old GPS coordinate, we need to convert it to the new format.
                        convertGpsV1ToGpsCoordinate text

                    GpsV2 tuple ->
                        -- No conversion needed here
                        tuple
            )
            (\value -> GpsV2 value)

convertGpsV1ToGpsCoordinate : String -> (Float, Float)
convertGpsV1ToGpsCoordinate =
    Debug.todo "Add the conversion code"
```
If we decide to make more changes to our GPS coordinate, we can safely just append more variants to act as versions.
The crucial thing is that we had this versioning system set up from the beginning. If we had just written 
```elm
gpsCodec : GpsCoordinate
gpsCodec = S.string
``` 
then we wouldn't be able to add a new version later.

## Credits

This package is an iteration on `MartinSStewart/elm-codec-bytes` which is in turn inspired by `miniBill/elm-codec`.

Also thanks to [jfmengels](https://github.com/jfmengels) and [drathier](https://github.com/drathier) for providing lots of feedback!