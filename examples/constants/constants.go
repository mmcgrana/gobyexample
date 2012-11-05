// Go supports _constants_ of character, string, boolean,
// and numeric values.

package main

import "fmt"

// Use `const` to declare a constant value.
const s string = "constant"

func main() {
    fmt.Println(s)

    // A `const` statement can appear anywhere a `var` statement can.
    const n = 500000000

    // Constant expressions perform arithmetic with arbitrary precision.
    const d = 3e20 / n

    // A numeric constant has no type until it's given one, such as by
    // an explicit cast.
    fmt.Println(int64(d))

    // A number can also be given a type by using it in a context that
    // requires one, such as a variable assignment or funcion call.
    // The type it gets depends on its value.
    fmt.Println(n) // int
    fmt.Println(d) // float64
}
