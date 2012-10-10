// We can use channels to synchronize execution
// accross goroutines. Here's an example of waiting
// for another goroutine to finish.

package main

import "fmt"
import "time"

// The `done` channel will be used for
// synchronization.
func worker(done chan<- bool) {
    fmt.Print("Working...")
    time.Sleep(time.Second)
    fmt.Println(" done")

    // Send a value to notify that the work is done.
    done <- true
}

func main() {
    // Start a worker goroutine, give it the channel to
    // notify on.
    done := make(chan bool, 1)
    go worker(done)

    // Block until we can receive a value from the worker
    // over the channel.
    <-done
}
