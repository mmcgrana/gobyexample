package main                     // We can use channels to synchronize execution
                                 // accross goroutings. Here's an example of waiting
import "fmt"                     // for another gorouting to finish.
import "time"

func worker(done chan<- bool) {  // The `done` channel will be used for
    fmt.Print("Working...")      // synchronization.
    time.Sleep(time.Second)
    fmt.Println(" done")
	done <- true                 // Send a value to notify that the work is done.
}

func main() {
	done := make(chan bool, 1)   // Start a worker goroutine, give it the channel to
    go worker(done)              // notify on.

	<- done                      // Block until we can receive a value from the worker
}                                // over the channel.

/*
$ go run synchronization.go      // If you commented out the `<- done` line, the
Working... done                  // program would exit before the `worker` even
*/                               // started.
