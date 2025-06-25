# `zeroval` doesn't change the `i` in `main`, but
# `zeroptr` does because it has a reference to
# the memory address for that variable.
$ go run pointers.go
initial i: 1
initial s: [1 2 3]
After zeroval(i, s)
i: 1
s: [4 2 3]
After zeroptr(&i, &s)
i: 0
s: [4 5 3]
pointer i: 0xc000098040
pointer s: &[4 5 3]
