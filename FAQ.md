## What's different between this and miniBill/elm-codec?
Besides the fact that this package is designed for `elm/bytes` instead of `elm/json` other notable differences are:
* `variantX` functions use `Int` instead of `String` for telling apart different constructors
* `CustomObject` fields don't need to be given names but their order matters 
* There is no `oneOf` or `optionalField`

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