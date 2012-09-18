package main

import ("fmt"; "crypto/sha1"; "encoding/hex")

func main() {
    h := sha1.New()
    h.Write([]byte("sha1 this string"))
    bs := h.Sum([]byte{})
	fmt.Println(hex.EncodeToString(bs))
}
