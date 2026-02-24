// Sometimes we'd like our Go programs to intelligently
// handle [Unix signals](https://en.wikipedia.org/wiki/Unix_signal).
// For example, we might want a server to gracefully
// shutdown when it receives a `SIGTERM`, or a command-line
// tool to stop processing input if it receives a `SIGINT`.
// Here's a modern way to handle signals using contexts.

package main

import (
	"context"
	"fmt"
	"os/signal"
	"syscall"
)

func main() {
	// `signal.NotifyContext` returns a context that's canceled
	// when one of the listed signals arrives.
	ctx, stop := signal.NotifyContext(
		context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer stop()

	// The program will wait here until one of the
	// configured signals is received.
	fmt.Println("awaiting signal")
	<-ctx.Done()

	// `context.Cause` reports why the context was canceled.
	// For a signal-triggered cancellation, this includes
	// the signal value.
	fmt.Println()
	fmt.Println(context.Cause(ctx))
	fmt.Println("exiting")
}
