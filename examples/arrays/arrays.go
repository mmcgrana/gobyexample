// In Go, an array is a numbered sequence of elements of a
// specific length.

package main

import "fmt"

func main() {

    // The type of elements and length are both part of
    // the array's type. Here we create an array `x` that
    // will hold exactly 5 ints.
    var x [5]int

    // By default an array is zero-valued, which for ints
    // means an array of `0`s.
    fmt.Println("emp:", x)

    // We can set a value at a given index using the
    // `array[index] = value` syntax, and get a value
    // with `array[index]`.
    x[4] = 100
    fmt.Println("set:", x)
    fmt.Println("get:", x[4])

    // Use this syntax to decalare and initalize an array
    // in one line.
    y := [5]int{1, 2, 3, 4, 4}
    fmt.Println("dcl:", y)

    // The builtin `len` returns the length of an array.
    fmt.Println("len:", len(y))

    // Array types are one-dimensional, but you can
    // compose types to build multi-dimensional data
    // structures.
    var twoD [2][3]int
    for i := 0; i < 2; i++ {
        for j := 0; j < 3; j++ {
            twoD[i][j] = i + j
        }
    }
    fmt.Println("2d: ", twoD)
}
