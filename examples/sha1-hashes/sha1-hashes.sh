# Running the program computes the hash and prints it in
# a human-readable hex format.
$ go run sha1-hashes.go
sha1 this string
cf23df2207d99a74fbe169e3eba035e633b65d94


# You can compute other hashes using a similar pattern to
# the one shown above. For example, to compute MD5 hashes
# import `crypto/md5` and use `md5.New()`.

# Note that if you need cryptographically secure hashes,
# you should carefully research
# [hash strength](http://en.wikipedia.org/wiki/Cryptographic_hash_function)!
