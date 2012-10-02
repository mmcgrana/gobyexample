package main

import b64 "encoding/base64"
import "fmt"

func main() {
    // The data we'll encode/decode.
    data := "abc123!?$*&()'-=@~"
    fmt.Println(data)
    fmt.Println()

    // Standard base64 encoding/decoding.
    sEnc := b64.StdEncoding.EncodeToString([]byte(data))
    fmt.Println(sEnc)
    sDec, _ := b64.StdEncoding.DecodeString(sEnc)
    fmt.Println(string(sDec))
    fmt.Println()

    // URL base64 encoding/decoding.
    uEnc := b64.URLEncoding.EncodeToString([]byte(data))
    fmt.Println(uEnc)
    uDec, _ := b64.URLEncoding.DecodeString(uEnc)
    fmt.Println(string(uDec))
}
