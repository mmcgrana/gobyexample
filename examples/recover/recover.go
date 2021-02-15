// A `recover` means recovering from a `panic`,
// stopping the panic from propagating upwards.

package main

import (
	"fmt"
)

func main() {
	recoverFromPanic(-1)

	// We see it because we recovered from a panic.
	fmt.Printf("Finished without panicing.")
}

func recoverFromPanic(i int) {
	defer func() {
		// recover is always defined in a defer.
		if r := recover(); r != nil {
			fmt.Println("Recovered. Error:\n", r)
		}
	}()

	fmt.Printf("About to process i=%d\n", i)

	if i < 0 {
		panic(fmt.Errorf("Accepting only"+
			" non-negative numbers but received %d", i))
	}

	// We won't see this because we paniced.
	fmt.Printf("Doing something with %d\n", i)
}
