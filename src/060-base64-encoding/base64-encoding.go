package main

import "encoding/base64"
import "fmt"

func main() {
    // The data we'll encode/decode.
    data := "abc123!?$*&()'-=@~"
    fmt.Println(data)
    fmt.Println()

    // Standard base64 encoding/decoding.
    sEnc := base64.StdEncoding.EncodeToString([]byte(data))
    fmt.Println(sEnc)
    sDec, _ := base64.StdEncoding.DecodeString(sEnc)
    fmt.Println(string(sDec))
    fmt.Println()

    // URL base64 encoding/decoding.
    uEnc := base64.URLEncoding.EncodeToString([]byte(data))
    fmt.Println(uEnc)
    uDec, _ := base64.URLEncoding.DecodeString(uEnc)
    fmt.Println(string(uDec))
}
