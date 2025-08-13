// To wait for multiple goroutines to finish, we can
// use a *wait group*.

package main

import (
	"fmt"
	"sync"
	"time"
)

// This is the function we'll run in every goroutine.
func worker(id int) {
	fmt.Printf("Worker %d starting\n", id)

	// Sleep to simulate an expensive task.
	time.Sleep(time.Second)
	fmt.Printf("Worker %d done\n", id)
}

func main() {

	// This WaitGroup is used to wait for all the
	// goroutines launched here to finish. Note: if a WaitGroup is
	// explicitly passed into functions, it should be done *by pointer*.
	var wg sync.WaitGroup

	// Launch several goroutines using `WaitGroup.Go`
	for i := 1; i <= 5; i++ {
		wg.Go(func() {
			worker(i)
		})
	}

	// Block until all the goroutines started by `wg` are
	// done. A goroutine is done when the function it invokes
	// returns.
	wg.Wait()

	// Note that this approach has no straightforward way
	// to propagate errors from workers. For more
	// advanced use cases, consider using the
	// [errgroup package](https://pkg.go.dev/golang.org/x/sync/errgroup).
}
