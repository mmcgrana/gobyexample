// Go supports _constants_ of character, string, boolean,
// and numeric values.
// We can use _iota_ to simulate C's enum or #define constant.

package main

import (
	"fmt"
	"strconv"
)
import "math"

// `const` declares a constant value.
const s string = "constant"

// Sepcial method to generate maximum of uint.
// Ref: http://blog.golang.org/constants
const MaxUint = ^uint(0)
const MaxUint32 = ^uint32(0)

// Here we simulate C's `enum` by Go's `iota`
type Season uint8

const (
	Spring = Season(iota)
	Summer
	Autumn
	Winner
)

// output function for Season variable
func (s Season) String() string {
	name := []string{"spring", "summer", "autumn", "winner"}
	i := uint8(s)
	switch {
	case i <= uint8(Winner):
		return name[i]
	default:
		return strconv.Itoa(int(i))
	}
}
func main() {
	fmt.Println(s)

	// A `const` statement can appear anywhere a `var`
	// statement can.
	const n = 500000000

	// Constant expressions perform arithmetic with
	// arbitrary precision.
	const d = 3e20 / n
	fmt.Println(d)

	// A numeric constant has no type until it's given
	// one, such as by an explicit cast.
	fmt.Println(int64(d))

	// A number can be given a type by using it in a
	// context that requires one, such as a variable
	// assignment or function call. For example, here
	// `math.Sin` expects a `float64`.
	fmt.Println(math.Sin(n))

	// Assign Season variable and print it
	s := Summer
	fmt.Println(s)
	// Assign invalid range Season variable and print it
	s = Season(9)
	fmt.Println(s)

	fmt.Println(MaxUint)
	fmt.Println(MaxUint32)
}
