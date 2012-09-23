// ## If/Else

package main

import "fmt"

func main() {
    // If/else is straight-forward. Note that there are no
    // enclosing parentheses around the condition.
    // Also, there is no ternary operator (`?`) in Go.
    fmt.Print("7 is ")
    if 7%2 == 0 {
        fmt.Println("even")
    } else {
        fmt.Println("odd")
    }
}
