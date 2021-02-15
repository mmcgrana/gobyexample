# Running this program will exit correctly,
# even though panic was called.
$ go run recover.go
About to process i=-1
Recovered. Error:
 Accepting only non-negative numbers but received -1
Finished without panicing.
# Note that, in Go it is idiomatic
# to use error-indicating return values wherever possible.
