// In the previous example we saw how to manage simple
// counter state using atomic operations. For more complex
// state we can use a _[mutex](http://en.wikipedia.org/wiki/Mutual_exclusion)_
// to safely access data across multiple goroutines.

package main

import (
	"fmt"
	"math/rand"
	"runtime"
	"sync"
	"sync/atomic"
	"time"
)

func main() {

	// For our example the `state` will be a map.
	var state = make(map[int]int)

	// This `mutex` will synchronize access to `state`.
	var mutex = &sync.Mutex{}

	// To compare the mutex-based approach with another
	// we'll see later, `ops` will count how many
	// operations we perform against the state.
	var ops int64 = 0

	// Here we start 100 goroutines to execute repeated
	// reads against the state.
	for r := 0; r < 100; r++ {
		go func() {
			total := 0
			for {

				// For each read we pick a key to access,
				// `Lock()` the `mutex` to ensure
				// exclusive access to the `state`, read
				// the value at the chosen key,
				// `Unlock()` the mutex, and increment
				// the `ops` count.
				key := rand.Intn(5)
				mutex.Lock()
				total += state[key]
				mutex.Unlock()
				atomic.AddInt64(&ops, 1)

				// In order to ensure that this goroutine
				// doesn't starve the scheduler, we explicitly
				// yield after each operation with
				// `runtime.Gosched()`. This yielding is
				// handled automatically with e.g. every
				// channel operation and for blocking
				// calls like `time.Sleep`, but in this
				// case we need to do it manually.
				runtime.Gosched()
			}
		}()
	}

	// We'll also start 10 goroutines to simulate writes,
	// using the same pattern we did for reads.
	for w := 0; w < 10; w++ {
		go func() {
			for {
				key := rand.Intn(5)
				val := rand.Intn(100)
				mutex.Lock()
				state[key] = val
				mutex.Unlock()
				atomic.AddInt64(&ops, 1)
				runtime.Gosched()
			}
		}()
	}

	// Let the 10 goroutines work on the `state` and
	// `mutex` for a second.
	time.Sleep(time.Second)

	// Take and report a final operations count.
	opsFinal := atomic.LoadInt64(&ops)
	fmt.Println("ops:", opsFinal)

	// With a final lock of `state`, show how it ended up.
	mutex.Lock()
	fmt.Println("state:", state)
	mutex.Unlock()
}
