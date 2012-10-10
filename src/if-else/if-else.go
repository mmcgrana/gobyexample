// If/else in Go is straight-forward.

package main

import "fmt"

func main() {
    // If/else is straight-forward. Note that there are no
    // enclosing parentheses around the condition.
    // Also, there is no ternary operator (`?`) in Go.
    fmt.Print("7 is ")
    if 7%2 == 0 {
        fmt.Println("even")
    } else if 7%2 == 1 {
        fmt.Println("odd")
    } else {
        fmt.Println("???")
    }
}

// There is no ternary operator (i.e. `?`) in Go, so
// you'll need to use a full if/else even for very basic
// conditions.
