// [Timers](timers) are for when you want to do
// something once in the future - _tickers_ are for when
// you want to do something repeatedly at regular
// intervals. Here's an example of a ticker that ticks
// periodically until we stop it.

package main

import "time"
import "fmt"

func main() {

	// Tickers use a similar mechanism to timers: a
	// channel that is sent values. Here we'll use the
	// `range` builtin on the channel to iterate over
	// the values as they arrive every 500ms.
	ticker := time.NewTicker(time.Millisecond * 500)
	go func() {
		for t := range ticker.C {
			fmt.Println("Tick at", t)
		}
	}()

	// Tickers can be stopped like timers. Once a ticker
	// is stopped it won't receive any more values on its
	// channel. We'll stop ours after 1600ms.
	time.Sleep(time.Millisecond * 1600)
	ticker.Stop()
	fmt.Println("Ticker stopped")

	// Stop does not close the ticker channel to prevent
	// a read from the channel succeeding incorrectly,
	// so you can use an additional channel to avoid
	// a goroutine leak
	ticker2 := time.NewTicker(time.Millisecond * 500)
	tdone := make(chan bool, 1)
	go func() {
		for {
			select {
			case t := <-ticker2.C:
				fmt.Println("Tick at", t)
			case <-tdone:
				fmt.Println("x")
				ticker2.Stop()
				return
			}
		}
	}()

	time.Sleep(time.Millisecond * 1600)
	close(tdone)
	fmt.Println("Ticker stopped")
}
