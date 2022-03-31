# Running the program computes the hash and prints it in
# a human-readable hex format.
$ go run sha256-hashes.go
sha256 this string
1af1dfa857bf1d8814fe1af8983c18080019922e557f15a8a...


# You can compute other hashes using a similar pattern to
# the one shown above. For example, to compute
# SHA512 hashes import `crypto/sha512` and use
# `sha512.New()`.

# Note that if you need cryptographically secure hashes,
# you should carefully research
# [hash strength](https://en.wikipedia.org/wiki/Cryptographic_hash_function)!
