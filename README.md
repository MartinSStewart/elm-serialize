# elm-codec-bytes

This package allows you to build pairs of encoders (`a -> Encoder`) and decoders (`Decoder a`) for a sequence of bytes, collectively called a `Codec a`.

It is inspired by `miniBill/elm-codec` and reuses much of the API. 
It's not quite a drop in replacement however, see FAQ.md for a list of differences.

## Design Goals

The design goal is to be as type safe as possible while keeping a nice API.
Using this package will greatly reduce the risk of unmatched encoders and decoders.

The packages re-exposes the `Encoder`, `Decoder`, `Bytes`, and `Endianness` types from `elm/bytes`, so you don't need to import them too.

## Examples

### Basic usage ###

```elm
import Codec exposing (Bytes, Codec, Encoder)

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
