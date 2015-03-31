// A _goroutine_ is a lightweight thread of execution.

package main

import (
    "fmt"
    "runtime"
)

func f(from string) {
    for i := 0; i < 3; i++ {
        fmt.Println(from, ":", i)

        // The reason to call `runtime.GoSched()` is to transfer
        // execution to the next goroutine. This is necessary when
        // Go runs in a single system thread, since in that case
        // it won't yield to another goroutine unless asked for
        // with an explicit call as below or during a system call.
        // It's also possible to set the max number of system
        // threads with the `GOMAXPROCS` environment variable.
        runtime.Gosched()
    }
}

func main() {

    // Suppose we have a function call `f(s)`. Here's how
    // we'd call that in the usual way, running it
    // synchronously.
    f("direct")

    // To invoke this function in a goroutine, use
    // `go f(s)`. This new goroutine will execute
    // concurrently with the calling one.
    go f("goroutine")

    // You can also start a goroutine for an anonymous
    // function call.
    go func(msg string) {
        fmt.Println(msg)
    }("going")

    // Our two function calls are running asynchronously in
    // separate goroutines now, so execution falls through
    // to here. This `Scanln` code requires we press a key
    // before the program exits.
    var input string
    fmt.Scanln(&input)
    fmt.Println("done")
}
