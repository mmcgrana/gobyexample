# `zeroval` doesn't change the `i` in `main`, but
# `zeroptr` does because it has a reference to
# the memory address for that variable.
$ go run pointers.go
initial: 1
zeroval: 1
zeroptr: 0
pointer: 0x42131100
