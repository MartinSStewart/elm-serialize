# elm-serialize

Quickly and reliably write code to handle serialization of Elm data structures. 
This is done via `Codec`s which automatically create both the encoder and decoder ensuring that they don't get out of sync with eachother.

### What elm-serialize is good for?
- Serializing data that you want to save to, and retrieve from, local storage
- Communicating with a server that is also written in Elm
- Storing data in a reasonably space efficient format
    
### What elm-serialize is *not* good for?
- Decoding data not encoded by elm-serialize
- Encoding to a human readable format

## Basic usage

```elm
import Serialize as S

type alias MeAndMyFriends = 
    { name : String
    , friends : List String
    }

friendsCodec : S.Codec (List String)
friendsCodec =
    S.list S.string
    
meAndMyFriendsCodec : S.Codec MeAndMyFriends
meAndMyFriendsCodec =
    S.record Model 
        |> S.field .name S.string
        |> S.field .friends friendsCodec
        |> S.finishRecord

encode : MeAndMyFriends -> Bytes
encode meAndMyFriends =
    S.encodeToBytes meAndMyFriendsCodec meAndMyFriends

decode : Bytes -> Result S.Error MeAndMyFriends
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
        |> S.variant3 Red S.signedInt S.string S.bool
        |> S.variant1 Yellow S.float64
        |> S.variant0 Green
        |> S.finishCustom
```

## How do I change my data structures and still be able to decode data I've previously encoded?

First let's cover what counts as a breaking change for a Codec.

For records, adding or removing Serialize.field is a breaking change. 
Changing the Codec used in a Serialize.field is also a breaking change.

For custom types, removing a Serialize.variant is a breaking change. 
Changing one of the Codecs used in a Serialize.variant or changing how many Codecs are used is a breaking change.

Appending a Serialize.variant is *not* a breaking change.
```elm
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
    |> S.variant3 Red S.signedInt S.string S.bool
    |> S.variant1 Yellow S.float64
    |> S.variant0 Green
    -- We can safely add the new variant here at the end.
    |> S.variant0 Rainbow
    |> S.finishCustom
```
The example above will still decode anything encoded with the semaphoreCodec in the [custom types example](#Writing-codecs-for-custom-types)

Knowing this, what we can do is have a custom type that lets us handle different versions of our Codec.

Here's an example of what that would look like.

Suppose we are making an app that can serialize the users GPS coordinate. 
We are in a rush to get this app working so we just store GPS coordinates as a string. 
```elm
module AppCodec exposing (gpsCodec)

import Serialize as S

{-| The gps coordinate we use internally in our application
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
We still want to decode all the serialized data though so we change our AppCodec module to look like this:

```elm
module AppCodec exposing (gpsCodec)

import Serialize as S

{-| The gps coordinate we use internally in our application
-}
type alias GpsCoordinate =
    ( Float, Float )

type GpsVersions
    = GpsV1 String -- Old naive way of storing GPS coordinates
    | GpsV2 GpsCoordinate -- New better way

gpsV1Codec : S.Codec String
gpsV1Codec =
    S.string

gpsV2Codec : S.Codec GpsCoordinate
gpsV2Codec =
    S.tuple S.float64 S.float64

gpsCodec : S.Codec GpsCoordinate
gpsCodec =
    S.custom
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
        |> S.finishCustom
        |> S.map
            (\value ->
                case value of
                    GpsV1 text ->
                        -- After we've decoded an old GPS coordinate, we need to convert it to the new format.
                        convertGpsV1ToGpsCoordinate text

                    GpsV2 tuple ->
                        tuple -- No conversion needed here
            )
            (\value -> GpsV2 value)
            
convertGpsV1ToGpsCoordinate =
    Debug.todo "Add the conversion code"
```
If we decide to make more changes to our GPS coordinate, we can safely just append more variants to act as versions.
The crucial thing is that we had this versioning system set up from the beginning. If we had just written 
```elm
module AppCodec exposing (gpsCodec)

gpsCodec : GpsCoordinate
gpsCodec = S.string
``` 
then we wouldn't be able to add a new version later.

Okay, this system is pretty robust but it's also a lot of code to write. What else can we do?

Here's a list of little tricks you can do to handle changing data structures

- If you have a record and you want to remove a field from it, you can keep the Serialize.field 

## Credits

This package is inspired by `miniBill/elm-codec`.