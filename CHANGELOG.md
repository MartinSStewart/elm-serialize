### 1.1.0
* Added `bytes` and `lazy`.

### 2.0.0 

While this is a breaking change for the API, it is not a breaking change for the underlying binary format. 
If you've encoded data with version 1.1.0, 2.0.0 should be able to decode it as long as you replace any removed functions with the functions intended to replace them.

* Removed `recursive`. `lazy` is easier to use and more flexible. 
* Removed `signedInt` and `unsignedInt`. Use `signedInt32` and `unsignedInt32` instead.
* Removed the need to specify endianness for `signedInt32`, `unsignedInt32`, `signedInt16`, and `unsignedInt16`.
* Renamed `encoder` and `decoder` to `getEncoder` and `getDecoder`
* Renamed `encodeToValue` and `decodeValue` to `encoder` and `decoder`