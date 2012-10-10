package main

// Package `crypto/sha1` computes SHA1 hashes.
import "crypto/sha1"
import "encoding/hex"
import "fmt"

func main() {
    // The pattern is `sha1.New()`, `sha1.Write(bytes)`,
    // then `sha1.Sum([]byte{})
    h := sha1.New()

    // `Write` expects bytes. If you have a string `s`
    // use `[]byte(s)` to coerce it.
    h.Write([]byte("sha1 this string"))

    // Get the result. The argument to `Sum` can be used
    // to append to an existing buffer: usually uneeded.
    bs := h.Sum(nil)

    // SHA1 values are often printed in hex, for example
    // with git.
    fmt.Println(hex.EncodeToString(bs))
}

// You can compute other hashes using a similar
// pattern. For exmpale, to compute MD5 hashes
// import `crypto/md5` and use `md5.New()`.
