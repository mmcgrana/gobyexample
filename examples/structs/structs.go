// Go's _structs_ are typed collections of fields.
// They're useful for grouping data together to form
// records.

package main

import "fmt"

// This `person` struct type has `name` and `age` fields.
type person struct {
	name string
	age  int
}

// `newPerson` constructs a new person struct with the given name.
func newPerson(name string) *person {
	// You can safely return a pointer to local variable
	// as a local variable will survive the scope of the function.
	p := person{name: name}
	p.age = 42
	return &p
}

func main() {

	// This syntax creates a new struct.
	fmt.Println(person{"Bob", 20})

	// You can name the fields when initializing a struct.
	fmt.Println(person{name: "Alice", age: 30})

	// Omitted fields will be zero-valued.
	fmt.Println(person{name: "Fred"})

	// An `&` prefix yields a pointer to the struct.
	fmt.Println(&person{name: "Ann", age: 40})

	// It's idiomatic to encapsulate new struct creation in constructor functions
	fmt.Println(newPerson("Jon"))

	// Access struct fields with a dot.
	s := person{name: "Sean", age: 50}
	fmt.Println(s.name)

	// You can also use dots with struct pointers - the
	// pointers are automatically dereferenced.
	sp := &s
	fmt.Println(sp.age)

	// Structs are mutable.
	sp.age = 51
	fmt.Println(sp.age)
}
