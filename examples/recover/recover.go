// A `recover` means recovering from a `panic`, either from a "business" or "built-in" panic.
// We want to recover if we want to handle a panic, stopping it from propagating upwards.

package main

import (
	"fmt"
)

func main() {

	recoverFromBuiltInPanic(10)

	fmt.Println()

	recoverFromCustomPanic(-1)
}

func recoverFromBuiltInPanic(i int) {
	// defer is defined.
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Recovered. Error:\n", r)
		}
	}()

	var a [5]int
	fmt.Printf("Getting index %d"+
		" of array of len %d...\n", i, len(a))
	fmt.Printf("Item in index %d: %d", i, a[i])
}

func recoverFromCustomPanic(i int) {
	// defer is defined.
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Recovered. Error:\n", r)
		}
	}()

	fmt.Printf("About to process i=%d\n", i)

	if i < 0 {
		panic(fmt.Errorf("Accepting only"+
			" non-negative numbers but received %d", i))
	}

	fmt.Printf("Doing something with %d\n", i)
}
