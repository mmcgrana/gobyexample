// Use `os.Exit` to immediatly exit with a given
// status.

package main

import "os"

func main() {
    // `defer`s will _not_ be run when using `os.Exit`, so
    // this `println` will never be called.
    defer println("!")

    // Exit with status 3.
    os.Exit(3)
}

// Note that unlike e.g. C, Go does not use an integer
// return value from `main` to indicate exit status. If
// you'd like to exit with a non-zero status you should
// use `os.Exit`.

// todo: discuss building before getting here
