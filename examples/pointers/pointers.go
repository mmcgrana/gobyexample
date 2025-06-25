// Go supports <em><a href="https://en.wikipedia.org/wiki/Pointer_(computer_programming)">pointers</a></em>,
// allowing you to pass references to values and records
// within your program.

package main

import "fmt"

// We'll show how pointers work in contrast to values with
// 2 functions: `zeroval` and `zeroptr`. `zeroval` has an
// `int` parameter, so arguments will be passed to it by
// value. `zeroval` will get a copy of `ival` distinct
// from the one in the calling function.
// The second parameter is a `slice` and may seem to be an
// exception to the rule, but it is not. Read this blog <em><a href="https://go.dev/blog/slices-intro">post</a></em>
// if you want to know why.
func zeroval(ival int, sval []int) {
	ival = 0
	sval[0] = 4
}

// `zeroptr` in contrast has an `*int` parameter, meaning
// that it takes an `int` pointer. The `*iptr` code in the
// function body then _dereferences_ the pointer from its
// memory address to the current value at that address.
// Assigning a value to a dereferenced pointer changes the
// value at the referenced address.
// When indexing a pointer to a value, we have to use
// parenthesis because of <em><a href="https://go.dev/ref/spec#Operator_precedence">precedence</a></em>
func zeroptr(iptr *int, sval *[]int) {
	*iptr = 0
	(*sval)[1] = 5
}

func main() {
	i := 1
	s := []int{1, 2, 3}

	fmt.Println("initial i:", i)
	fmt.Println("initial s:", s)

	zeroval(i, s)
	fmt.Println("After zeroval(i, s)")
	fmt.Println("i:", i)
	fmt.Println("s:", s)

	// The `&i` syntax gives the memory address of `i`,
	// i.e. a pointer to `i`.
	zeroptr(&i, &s)
	fmt.Println("After zeroptr(&i, &s)")
	fmt.Println("i:", i)
	fmt.Println("s:", s)

	// Pointers can be printed too.
	fmt.Println("pointer i:", &i)
	fmt.Println("pointer s:", &s)
}
