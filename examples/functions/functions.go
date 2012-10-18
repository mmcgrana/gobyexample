// _Functions_ are central in Go. We'll learn about
// functions with a few different examples.

package main

import "fmt"

// Here's a function that takes two `int`s and returns
// their sum as an `int`.
func plus(a int, b int) int {

    // Go requires explicit returns, i.e. it won't
    // automatically return the value of the last
    // expression.
    return a + b
}

func main() {

    // Call a function just as you'd expect, with
    // `name(args)`.
    res := plus(1, 2)
    fmt.Println("1+2 =", res)
}

// todo: coalesced parameter types
