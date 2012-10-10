// Funcations are critical in Go as in any other language.
// We'll look at some basic examples first.

package main

import "fmt"

// The syntax is `func name(args) returns { body }`. Note
// the the types of arguments come after their name.
// In this case we're taking a slice of floats and
// returning a single float value.
func avg(vals []float64) float64 {
    total := 0.0
    for _, val := range vals {
        total += val
    }
    // Go requires an expliciti return, i.e. it won't
    // automatically return the value of the last
    // expression.
    return total / float64(len(vals))
}

func main() {
    input := []float64{98, 93, 77, 82, 83}
    fmt.Println(input)
    // Call a function just as you'd expect, with
    // `name(args)`.
    output := avg(input)
    fmt.Println(output)
}

// There are several other features to Go functions,
// including multiple return values and varargs, which
// we'll look at next.
