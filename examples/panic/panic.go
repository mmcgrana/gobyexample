// A `panic` typically means something went unexpectedly
// wrong. Mostly we use it to fail fast on errors that
// shouldn't occur during normal operation, or that we
// aren't prepared to handle gracefully.

package main

import (
	"os"
	"path/filepath"
)

func main() {

	// We'll use panic throughout this site to check for
	// unexpected errors. This is the only program on the
	// site designed to panic.
	panic("a problem")

	// Note: The following code is unreachable because of the
    // panic above, but it demonstrates panicking on unexpected
    // errors when creating a file.
	// A common use of panic is to abort if a function
	// returns an error value that we don't know how to
	// (or want to) handle. Here's an example of
	// `panic`king if we get an unexpected error when creating a new file.
	_, err := os.Create(filepath.Join(os.TempDir(), "file"))
	if err != nil {
		panic(err)
	}
}
