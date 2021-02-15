# Running this program will exit correctly,
# even though panic was invoked in two methods.
# The recover is responsible for recovering from panics.
$ go run recover.go
Getting index 10 of array of len 5...
Recovered. Error:
 runtime error: index out of range [10] with length 5

About to process i=-1
Recovered. Error:
 Accepting only non-negative numbers but received -1
# Note that, in Go it is idiomatic
# to use error-indicating return values wherever possible.
