// [_Rate limiting_](https://en.wikipedia.org/wiki/Rate_limiting)
// is an important mechanism for controlling resource
// utilization and maintaining quality of service. Go
// elegantly supports rate limiting with goroutines,
// channels, and [tickers](tickers).

package main

import (
	"fmt"
	"time"
)

func main() {

	// First we'll look at basic rate limiting. Suppose
	// we want to limit our handling of incoming requests.
	// We'll serve these requests off a channel of the
	// same name.
	requests := make(chan int, 5)
	for i := 1; i <= 5; i++ {
		requests <- i
	}
	close(requests)

	// This `limiter` channel will receive a value
	// every 200 milliseconds. This is the regulator in
	// our rate limiting scheme.
	limiter := time.Tick(200 * time.Millisecond)

	// By blocking on a receive from the `limiter` channel
	// before serving each request, we limit ourselves to
	// 1 request every 200 milliseconds.
	for req := range requests {
		<-limiter
		fmt.Println("request", req, time.Now())
	}

	// We may want to allow short bursts of requests in
	// our rate limiting scheme while preserving the
	// overall rate limit. We can accomplish this by
	// buffering our limiter channel. This `burstyLimiter`
	// channel will allow bursts of up to 3 events.
	burstyLimiter := make(chan time.Time, 3)

	// Fill up the channel to represent allowed bursting.
	for i := 0; i < 3; i++ {
		burstyLimiter <- time.Now()
	}

	// Every 200 milliseconds we'll try to add a new
	// value to `burstyLimiter`, up to its limit of 3.
	go func() {
		for t := range time.Tick(200 * time.Millisecond) {
			burstyLimiter <- t
		}
	}()

	// Now simulate 5 more incoming requests. The first
	// 3 of these will benefit from the burst capability
	// of `burstyLimiter`.
	burstyRequests := make(chan int, 5)
	for i := 1; i <= 5; i++ {
		burstyRequests <- i
	}
	close(burstyRequests)
	for req := range burstyRequests {
		<-burstyLimiter
		fmt.Println("request", req, time.Now())
	}
}
