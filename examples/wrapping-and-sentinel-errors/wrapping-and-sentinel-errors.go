// Wrapping errors is a technique that allows you to add additional context
// to an error while preserving the original error.
// This approach is beneficial for debugging and understanding
// the chain of events that led to an error, especially in
// complex applications with multiple layers of function calls.

package main

import (
	"errors"
	"fmt"
	"math/rand/v2"
)

// A "sentinel error" has usualy the prefix "Err"
var ErrOutOfTea = errors.New("no more tea available.")

var ErrPower = errors.New("can't boil water.")

func MakeTea() error {
	if rand.Int32N(4) == 0 {
		return ErrOutOfTea
	}
	if rand.Int32N(7) == 0 {
		// You can wrap a sentinel error with %w
		return fmt.Errorf("Add custom text: %w", ErrPower)
	}
	return nil
}

func main() {
	for range 14 {
		err := MakeTea()
		if err != nil {
			// By using several if-statements we can handle
			// different sentinel errors.
			// A switch statement is not applicable here.
			if errors.Is(err, ErrOutOfTea) {
				fmt.Println("We should buy new tea!")
				continue
			}
			if errors.Is(err, ErrPower) {
				fmt.Println("Now it is dark.")
				continue
			}
			fmt.Printf("Some unknown error: %s", err)
			continue
		}
		fmt.Println("The tea is warm and inviting.")
	}
}
