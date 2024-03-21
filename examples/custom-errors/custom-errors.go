// It's possible to use custom types as `error`s by
// implementing the `Error()` method on them. Here's a
// variant on the example above that uses a custom type
// to explicitly represent an argument error.

package main

import (
	"errors"
	"fmt"
)

// It's possible to use custom types as `error`s by
// implementing the `Error()` method on them. Here's a
// variant on the example above that uses a custom type
// to explicitly represent an argument error.
// A custom error type has usualy the suffix "Error".
type argError struct {
	arg     int
	message string
}

func (e *argError) Error() string {
	return fmt.Sprintf("%d - %s", e.arg, e.message)
}

func f(arg int) (int, error) {
	if arg == 42 {
		// In this case we use `&argError` syntax to build
		// a new struct, supplying values for the two
		// fields `arg` and `message`.
		return -1, &argError{arg, "can't work with it"}
	}
	return arg + 3, nil
}

func main() {
	// If you want to programmatically use the data in
	// a custom error, you'll need to get the error as an
	// instance of the custom error with errors.As()
	_, err := f(42)
	var ae *argError
	if errors.As(err, &ae) {
		fmt.Println(ae.arg)
		fmt.Println(ae.message)
	}
}
