// A _goroutine_ is a lightweight thread of execution.

package main

import (
    "fmt"
    "runtime"
)

func f(from string) {
    for i := 0; i < 3; i++ {
        fmt.Println(from, ":", i)

        // `runtime.GoSched()` transfers execution to the next
        // goroutine. Since goroutines are multiplexed across
        // system threads, if there's only one thread it won't
        // yield to another goroutine unless asked explicitly
        // as with the call as below or during a system call.
        // Note that you can control the number of system
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
