// `for` is Go's only looping construct. Here are
// two common forms.
package main

import "fmt"

func main() {

    // Initialize `i` with `1` and loop until it's 10.
    i := 1
    for i <= 3 {
        fmt.Print(i)
        i = i + 1
    }

    // That type of loop is common. We can do it on one
    // line.
    for j := 1; j <= 3; j++ {
        fmt.Print(j)
    }

    // `for` without a condition will loop until you
    // `return`.
    for {
        fmt.Println()
        return
    }
}

// We'll see other `for` forms latter.

// todo: break out of for loop?
