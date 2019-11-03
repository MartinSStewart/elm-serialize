### 1.1.0
* Added `bytes` and `lazy`.

### 2.0.0 

This is both a breaking change for the API and any data encoded with it. It's possible to decode old data if you handle the following changes:


* `variant0`, `variant1`, etc for `CustomCodec` no longer need an id. In order to upgrade, follow this example
    ```elm
    type MyType = MyType0 | MyType1
    
    myCodec = 
        Codec.custom
            (\myType0Encoder myType1Encoder value ->
                case value of
                    MyType0 ->
                        myType0Encoder
    
                    MyType1 ->
                        myType1Encoder
            )
            |> Codec.variant0 0 MyType0
            |> Codec.variant0 1 MyType1
            |> Codec.buildCustom
    ```
    becomes
    ```elm
    type MyType = MyType0 | MyType1
    
    myCodec = 
        Codec.customWithIdCodec Codec.signedInt32
            (\myType0Encoder myType1Encoder value ->
                case value of
                    MyType0 ->
                        myType0Encoder
    
                    MyType1 ->
                        myType1Encoder
            )
            |> Codec.variant0 MyType0
            |> Codec.variant0 MyType1
            |> Codec.buildCustom
    ```
    If your ids don't start at 0 or aren't incrementally increasing **then it's not possible to upgrade**. If this is a serious problem, post an issue on GitHub and we'll figure out something.
* Removed `recursive`. `lazy` is easier to use and more flexible. 
* Removed `signedInt` and `unsignedInt`. Use `signedInt32` and `unsignedInt32` instead.
* Removed the need to specify endianness for `signedInt32`, `unsignedInt32`, `signedInt16`, and `unsignedInt16`.
* Renamed `encoder` and `decoder` to `getEncoder` and `getDecoder`
* Renamed `encodeToValue` and `decodeValue` to `encoder` and `decoder`