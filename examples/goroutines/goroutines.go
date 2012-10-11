// A _goroutine_ is a lightweight thread of execution.

package main

import "fmt"

func f(from string) {
    for i := 0; i < 3; i++ {
        fmt.Println(from, ":", i)
    }
}

func main() {

    // Suppose we have a function call `f(s)`. Here's how
    // we'd call that in the usual way, running it inline.
    f("direct")

    // To invoke this function in a goroutine, use
    // `go f(s)`. This will start a new, concurrently
    // executing goroutine.
    go f("goroutine")

    // You can also start a goroutine for an anonymous
    // function.
    go func() {
        fmt.Println("going")
    }()

    // Since our two goroutines are running asynchrously
    // in separate goroutines now, execution immediatly
    // falls through to here. This `Scanln` code requires
    // that we press a key before the program exits.
    var input string
    fmt.Scanln(&input)
    fmt.Println("done")
}
