                     // Panic

                     // A `panic` means something went unexpectedly wrong.
                     // Mostly we use it to fail fast on errors that
                     // shouldn't occur during normal operation.
package main

func main() {
	panic("O noes")  // We'll use panic throught this book to check for
}	                 // unexpected errors. This is the only program in the
                     // book designed to panic.

/*
$ go run 26-panic.go
panic: O noes

goroutine 1 [running]:
main.main()
	/.../src/26-panic.go:4 +0x47
...
*/
