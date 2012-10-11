# Note that while slices are different types than arrays,
# they are rendered similarly by `fmt.Println`.
$ go run slices.go
emp: [0 0 0 0 0]
set: [0 0 0 0 100]
get: 100
len: 5
apd: [0 0 0 0 100 6 7 8]
cpy: [0 0 0 0 100 6 7 8]
sl1: [100 6 7]
sl2: [0 0 0 0 100 6 7]
sl3: [100 6 7 8]
dcl: [1 2 3 4 5]
2d:  [[0] [1 2] [2 3 4]]

# Check out this [great blog post](http://blog.golang.org/2011/01/go-slices-usage-and-internals.html)
# by the Go team for more details on the design and
# implementation of slices in Go.

# Now that we've seen arrays and slices we'll look at
# Go's other key builtin data structure: maps.
