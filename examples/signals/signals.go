// Sometimes we'd like our Go programs to intelligently
// handle [Unix signals](http://en.wikipedia.org/wiki/Unix_signal).
// For example, we might want a server to gracefully
// shutdown when it receives a `SIGTERM`, or a command-line
// tool to stop processing input if it receives a `SIGINT`.
// Here's how to handle signals in Go with channels.

package main

import (
	"context"
	"fmt"
	"os/signal"
	"syscall"
)

func main() {

	// Go signal notification works by sending `os.Signal`
	// values on a channel.

	// `signal.NotifyContext` registers a channel under the hood to receive
	// notifications of the specified signals.
	// When a specified signal is received, it returns a context with a closed Done channel.
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer stop()

	// The program will wait here until the returned context's Done channel is closed.
	// A graceful exit can then be handled before the deferred stop is executed.
	// `stop` restores the default behavior of the signal (e.g., exiting on SIGINT)
	fmt.Println("awaiting signal")
	<-ctx.Done()
	fmt.Println("exiting gracefully")

	// Note: For cases where decisions based on which signal was received are needed,
	// `signal.Notify` and `context.WithCancel` can be used together instead.
}
