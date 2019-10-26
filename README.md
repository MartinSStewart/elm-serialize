# elm-codec-bytes

The goal of this package is to easily convert Elm values to a sequence of 
bytes and back without failing to decode or ending up with different values.

Note that this package is _not_ appropriate if you want to handle an external binary format.

## Basic usage

Round-tripping is done through the use of encoders (`a -> Encoder`) and decoders (`Decoder a`) for a sequence of bytes, collectively called a `Codec a`.

```elm
import Codec.Bytes as Codec exposing (Bytes, Codec, Encoder)

codec : Codec (List Int)
codec =
    Codec.list Codec.signedInt

encode : List Int -> Bytes
encode list =
    Codec.encodeToValue codec list

decode : Bytes -> Maybe (List Int)
decode s =
    Codec.decodeValue codec s
```

## Learning Resources

Ask for help on the [Elm Slack](https://elmlang.herokuapp.com/).

You can also have a look at the `FAQ.md` file.

## Credits

This package is inspired by `miniBill/elm-codec` and reuses much of the API. 
It's not a drop in replacement however, see `FAQ.md` for a list of differences.