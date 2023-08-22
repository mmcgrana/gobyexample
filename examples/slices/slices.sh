# Note that while slices are different types than arrays,
# they are rendered similarly by `fmt.Println`.
$ go run slices.go
uninit: [] true true
emp: [  ] len: 3 cap: 3
set: [a b c]
get: c
len: 3
apd: [a b c d e f]
cpy: [a b c d e f]
sl1: [c d e]
sl2: [a b c d e]
sl3: [c d e f]
dcl: [g h i]
t == t2
2d:  [[0] [1 2] [2 3 4]]

# Check out this [great blog post](https://go.dev/blog/slices-intro)
# by the Go team for more details on the design and
# implementation of slices in Go.

# Now that we've seen arrays and slices we'll look at
# Go's other key builtin data structure: maps.
