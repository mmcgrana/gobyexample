// [_SHA256 hashes_](https://en.wikipedia.org/wiki/SHA-2) are
// frequently used to compute short identities for binary
// or text blobs. For example, TLS/SSL certificates use SHA256
// to compute a certificate's signature. Here's how to compute
// SHA256 hashes in Go.

package main

// Go implements several hash functions in various
// `crypto/*` packages.
import (
	"crypto/sha256"
	"fmt"
)

func main() {
	s := "sha256 this string"

	// Here we start with a new hash.
	h := sha256.New()

	// `Write` expects bytes. If you have a string `s`,
	// use `[]byte(s)` to coerce it to bytes.
	h.Write([]byte(s))

	// This gets the finalized hash result as a byte
	// slice. The argument to `Sum` can be used to append
	// to an existing byte slice: it usually isn't needed.
	bs := h.Sum(nil)

	fmt.Println(s)
	fmt.Printf("%x\n", bs)
}
