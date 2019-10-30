### 1.1.0
* Added `bytes` and `lazy`.

### 2.0.0 
* Removed `recursive`. `lazy` is easier to use and more flexible. 
* Removed `signedInt` and `unsignedInt`
* Removed the need to specify endianness for `signedInt32`, `unsignedInt32`, `signedInt16`, and `unsignedInt16`.
* Renamed `encoder` and `decoder` to `getEncoder` and `getDecoder`
* Renamed `encodeToValue` and `decodeValue` to `encoder` and `decoder`
