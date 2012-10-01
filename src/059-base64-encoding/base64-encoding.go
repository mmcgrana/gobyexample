package main

import "encoding/base64"
import "fmt"

func main() {
	// The data we'll encode/decode.
	data := "abc123!?$*&()'-=@~"
	fmt.Println(data)
	fmt.Println()

	// Standard base64 encoding/decoding.
	stdEnc := base64.StdEncoding.EncodeToString([]byte(data))
	fmt.Println(stdEnc)
	stdDec, _ := base64.StdEncoding.DecodeString(stdEnc)
	fmt.Println(string(stdDec))
	fmt.Println()

	// URL base64 encoding/decoding.
	urlEnc := base64.URLEncoding.EncodeToString([]byte(data))
	fmt.Println(urlEnc)
	urlDec, _ := base64.URLEncoding.DecodeString(urlEnc)
	fmt.Println(string(urlDec))
}
