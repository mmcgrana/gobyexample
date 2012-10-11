// Go's _select_ lets you wait on multiple channel
// operations. Combining goroutines and channels with
// select is powerful feature of Go.

package main

import "time"
import "fmt"

func main() {

    // For our example we'll select accross two channels.
    c1 := make(chan string)
    c2 := make(chan string)

    // This third channel will indicate when we're done
    // and can exit the program.
    d := make(chan bool, 1)

    // The first two channels will receive a value after
    // some amount of time, to simulate e.g. blocking RPC
    // operations executing in concurrent goroutines.
    go func() {
        time.Sleep(time.Second * 1)
        c1 <- "one"
    }()
    go func() {
        time.Sleep(time.Second * 2)
        c2 <- "two"
    }()

    // We'll use `select` to await both of these values
    // simultaneously, printing each one as it arrives.
    // Once we've received both, we'll send a value to the
    // `d` channel indicating that we're ready to proceed.
    go func() {
        for i := 0; i < 2; i++ {
            select {
            case msg1 := <-c1:
                fmt.Println("received", msg1)
            case msg2 := <-c2:
                fmt.Println("received", msg2)
            }
        }
        d <- true
    }()

    // Since all other code is running in concurrent
    // goroutines, we'll executing a blocking receive
    // here to await completion of our example.
    <-d
}
