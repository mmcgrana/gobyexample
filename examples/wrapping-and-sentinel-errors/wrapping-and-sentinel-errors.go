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

// A sentinel error is a predeclared error variable that is used to
// signify a specific error condition. It allows error values to be compared
// directly via errors.Is() specific types of errors.
var ErrOutOfTea = fmt.Errorf("no more tea available")

var ErrPower = fmt.Errorf("can't boil water")

func MakeTea() error {
	if rand.Int32N(4) == 0 {
		return ErrOutOfTea
	}
	if rand.Int32N(7) == 0 {
		// You can wrap an error with %w
		return fmt.Errorf("add custom text: %w", ErrPower)
	}
	return nil
}

func main() {
	err := makeTeaSeveralTimes()
	if err != nil {
		fmt.Println("One or serveral errors occured")
	}
}

func makeTeaSeveralTimes() error {
	var allErrs []error
	for range 14 {
		err := MakeTea()
		if err != nil {
			allErrs = append(allErrs, err)
			// errors.Is is a function in Go that checks if a given error
			// matches a specific error value. It's used to determine whether
			// an error (or any error in its chain) is equivalent to a particular
			// target error. This is especially useful with wrapped or nested errors,
			// allowing you to identify specific error types or sentinel errors in
			// a chain of errors.
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

	// Several errors can be joined to one error.
	return errors.Join(allErrs...)
}
