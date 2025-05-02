// _Type assertion_ A type assertion provides access to an interface value's
// underlying concrete value.
package main

import "fmt"

func main() {

	// Create a new variable i of interface type with underlying type string
	var i interface{} = "hello"

	// Print the value of will contais string value
	fmt.Printf("Value i=%v, Type is %T\n", i, i)

	// Type assertion can return two values: underlying value and boolean value
	// whether the assertion succeeded.
	s, ok := i.(string)
	fmt.Printf("Correct type s=%v, ok=%v\n", s, ok)

	// if assertin applied to the wrong type Type assertion return zero value of
	// asseted Type and boolean will be false
	f, ok := i.(float64)
	fmt.Printf("Wrong type f=%v, ok=%v\n", f, ok)

	// if assertin applied to the wrong type and boolean return value omited
	// panic occurs
	// f = i.(float64)
}
