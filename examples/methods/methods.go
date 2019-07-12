// Go supports _methods_ defined on struct types.

package main

import "fmt"

type rect struct {
	width, height int
	label         string
}

// This `area` method has a _pointer receiver type_ `*rect`.
func (r *rect) area() int {
	return r.width * r.height
}

// Methods can be defined for either pointer or value
// receiver types. This method `perim` of _value receiver type_
// will pass the `rect` struct by value instead of reference for
// each call.
func (r rect) perim() int {
	return 2*r.width + 2*r.height
}

// The method `setLabel` will modify the rect's (pointer receiver)
// label value.
func (r *rect) setLabel(label string) {
	r.label = label
}

func main() {
	r := rect{width: 10, height: 5}

	// Here we call the 2 methods defined for our struct.
	fmt.Println("area: ", r.area())
	fmt.Println("perim:", r.perim())

	// Go automatically handles conversion between values
	// and pointers for method calls. You may want to use
	// a pointer receiver type to avoid copying on method
	// calls or to allow the method to mutate the
	// receiving struct.
	rp := &r
	fmt.Println("area: ", rp.area())
	fmt.Println("perim:", rp.perim())

	// Consider what may happen if `setLabel` worked on a value
	// receiver as opposed to a pointer receiver. Generally,
	// pointer receivers are more efficient and more useful so
	// are more common than value receiver methods.
	fmt.Println("label:", r.label)
	rp.setLabel("Rectangle")
	fmt.Println("label:", r.label)
}
