package main

import "fmt"

type Point struct {
	x, y int
}

func main() {
	point := Point{1, 2}
	
	fmt.Printf("default: %v\n", point)
	fmt.Printf("default w/ vals: %+v\n", point)
	fmt.Printf("go: %#v\n", point)
	fmt.Printf("go type: %T\n", point)

	fmt.Printf("boolean: %t\n", true)
}

// == todo
// moar

// Integer:
// 
// %b	base 2
// %c	the character represented by the corresponding Unicode code point
// %d	base 10
// %o	base 8
// %q	a single-quoted character literal safely escaped with Go syntax.
// %x	base 16, with lower-case letters for a-f
// %X	base 16, with upper-case letters for A-F
// %U	Unicode format: U+1234; same as "U+%04X"
// Floating-point and complex constituents:
// 
// %b	decimalless scientific notation with exponent a power of two,
// 	in the manner of strconv.FormatFloat with the 'b' format,
// 	e.g. -123456p-78
// %e	scientific notation, e.g. -1234.456e+78
// %E	scientific notation, e.g. -1234.456E+78
// %f	decimal point but no exponent, e.g. 123.456
// %g	whichever of %e or %f produces more compact output
// %G	whichever of %E or %f produces more compact output
// String and slice of bytes:
// 
// %s	the uninterpreted bytes of the string or slice
// %q	a double-quoted string safely escaped with Go syntax
// %x	base 16, lower-case, two characters per byte
// %X	base 16, upper-case, two characters per byte
// Pointer:
// 
// %p	base 16 notation, with leading 0x
