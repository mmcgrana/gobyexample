// ## Multiple Return Values

// In Go, a function can return multiple values.
// This feature is used often, for example to
// return a result and an error from a function.

package main

import "fmt"

// The `(int, int)` in this signature shows that the
// function returns 2 ints.
func vals() (int, int) {
    return 3, 7

}

func main() {
    // Use the 2 different return values from the call,
    // i.e. multiple assignement.
    x, y := vals()
    fmt.Println(x)
    fmt.Println(y)
}
