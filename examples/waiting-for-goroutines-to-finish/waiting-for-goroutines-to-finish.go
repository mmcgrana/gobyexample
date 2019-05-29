// To wait for multiple goroutines to finish, we can
// use a sync.WaitGroup.

package main

import (
    "fmt"
    "math/rand"
    "sync"
    "time"
)

// This is the function we'll run in every goroutine.
// wg is the WaitGroup it uses to notify that it's done.
// Note that a WaitGroup must be passed to functions by
// pointer.
func worker(id int, wg *sync.WaitGroup) {
    fmt.Printf("Worker %d starting\n", id)

    // Sleep for a random duration between 500-700 ms
    // to simulate work. See the [random numbers](random-numbers)
    // example for more details on *rand*.
    msToSleep := time.Duration(500 + rand.Intn(200))
    time.Sleep(msToSleep * time.Millisecond)
    fmt.Printf("Worker %d done\n", id)

    // Notify the WaitGroup that we're done.
    wg.Done()
}

func main() {

    // This WaitGroup is used to wait for all the
    // goroutines launched here to finish.
    var wg sync.WaitGroup

    // Launch several goroutines and increment the WorkGroup
    // counter for each.
    for i := 1; i <= 5; i++ {
        wg.Add(1)
        go worker(i, &wg)
    }

    // Block until the WorkGroup counter goes back to 0;
    // all the workers notified they're done.
    wg.Wait()
}
