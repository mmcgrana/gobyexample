	                                        // `crypto/sha1` computes SHA1 hashes.

package main

import (
	"fmt"
	"crypto/sha1"
	"encoding/hex"
)

func main() {
    h := sha1.New()                         // The basic pattern is `sha1.New()`, `sha1.Write(bytes)`, then
                                            // `sha1.Sum([]byte{})

    h.Write([]byte("sha1 this string"))     // `Write` expects bytes. If you have a string `s` use
											// `[]byte(s)` to coerce it.

    bs := h.Sum(nil)                        // Get the result. The argument to `Sum` can be used to append
                                            // to an existing buffer, but usually you won't need it.

	fmt.Println(hex.EncodeToString(bs))     // SHA1 values are often printed in hex, for example with git.
}

                                            // You can compute other hashes like MD5 using the same
                                            // pattern. Import `crypto/md5` and use `md5.New()`.
