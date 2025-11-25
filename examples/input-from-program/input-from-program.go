// Many programs need a quick way to pull input from the user.
// `fmt.Scanf` reads formatted data directly from standard input.
package main

import (
	"fmt"
)

func main() {
	var (
		name       string
		favNumber  int
		valuesRead int
		err        error
	)

	fmt.Print("Enter your name and favorite number: ")

	// `fmt.Scanf` stops at whitespace and stores parsed
	// values into the provided pointers. It also returns
	// how many items it scanned along with any error.
	valuesRead, err = fmt.Scanf(
		"%s %d",
		&name,
		&favNumber,
	)
	if err != nil {
		panic(err)
	}
	if valuesRead != 2 {
		panic("expected a string and a number")
	}

	fmt.Printf("\nNice to meet you, %s!\n", name)
	fmt.Printf("%d is a great choice.\n", favNumber)
}
