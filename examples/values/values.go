// Go tem vários tipos de valores, dentre eles:
// strings, integers, floats, booleans, etc.
// Aqui estão alguns exemplos básicos.

package main

import "fmt"

func main() {

	// Strings, que podem ser concatenadas usando `+`.
	fmt.Println("go" + "lang")

	// Integers e floats.
	fmt.Println("1+1 =", 1+1)
	fmt.Println("7.0/3.0 =", 7.0/3.0)

	// Booleans, com operadores booleanos.
	fmt.Println(true && false)
	fmt.Println(true || false)
	fmt.Println(!true)
}
