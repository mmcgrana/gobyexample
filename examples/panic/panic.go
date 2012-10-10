// A `panic` means something went unexpectedly wrong.
// Mostly we use it to fail fast on errors that
// shouldn't occur during normal operation.
package main

func main() {
    // We'll use panic throught this book to check for
    // unexpected errors. This is the only program in the
    // book designed to panic.
    panic("O noes")
}
