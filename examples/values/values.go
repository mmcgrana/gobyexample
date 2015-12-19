// Go ha una serie di valori di tipo differente,
// fra i quali anche string, integer, boolean,
// floats, etc. Vediamo insieme qualche esempio
// basilare su come usare questi tipi.

package main

import "fmt"

func main() {

	// String, che possono essere concatenate con `+`.
	fmt.Println("go" + "lang")

	// Integers e floats.
	fmt.Println("1+1 =", 1+1)
	fmt.Println("7.0/3.0 =", 7.0/3.0)

	// Booleans, con i classici operatori booleani
	// AND, OR e NOT.
	fmt.Println(true && false)
	fmt.Println(true || false)
	fmt.Println(!true)
}
