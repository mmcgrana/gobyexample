// Go supports
// <a href="https://en.wikipedia.org/wiki/Recursion_(computer_science)"><em>recursive functions</em></a>.
// Here's a classic example.

package main

import "fmt"

// This `fact` function calls itself until it reaches the
// base case of `fact(0)`.
func fact(n int) int {
	if n == 0 {
		return 1
	}
	return n * fact(n-1)
}

func main() {
	fmt.Println(fact(7))

	// Closures can also be recursive, but this requires the
	// closure to be declared with a typed `var` explicitly
	// before it's defined.
	var fib func(n int) int

	fib = func(n int) int {
		if n < 2 {
			return n
		}

		// Since `fib` was previously declared in `main`, Go
		// knows which function to call with `fib` here.
		return fib(n-1) + fib(n-2)
	}

	fmt.Println(fib(7))
}
