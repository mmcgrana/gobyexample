// _Slices_ are a key data type in Go, giving a more
// powerful interface to sequences than arrays.

package main

import "fmt"

func main() {

    // Unlike arrays, slices are typed only by the
    // elements they contain (not the number of elements).
    // To create an empty slice with non-zero length, use
    // the builtin `make`. Here we make a slice of `int`s
    // of length `5` (initially empty-valued).
    s := make([]int, 5)
    fmt.Println("emp:", s)

    // We can set and get just like with arrays.
    s[4] = 100
    fmt.Println("set:", s)
    fmt.Println("get:", s[4])

    // `len` returns the length of the slice as expected.
    fmt.Println("len:", len(s))

    // In addition to these basic operations, slices
    // support several more that make them richer than
    // arrays. One is the builtin `append`, which 
    // returns a slice containing one or more new values.
    // Note that we need to accapt a return value from
    // append as we may get a new slice reference.
    s = append(s, 6)
    s = append(s, 7, 8)
    fmt.Println("apd:", s)

    // Slices can also be `copy`'d. Here we create an
    // empty slice `c` of the same length as `s` and copy
    // into `c` from `s`.
    c := make([]int, len(s))
    copy(c, s)
    fmt.Println("cpy:", c)

    // Slices support a "slice" operator, which is denoted
    // with brackets containing `:`. For example, this
    // gets a slice of the elements 4, 5, and 6.
    l := s[4:7]
    fmt.Println("sl1:", l)

    // This slices up to the 7th index.
    l = s[:7]
    fmt.Println("sl2:", l)

    // And this slices from the 4th index upwards.
    l = s[4:]
    fmt.Println("sl3:", l)

    // We can declare and initalize a slice in a single
    // line as well.
    t := []int{1, 2, 3, 4, 5}
    fmt.Println("dcl:", t)

    // Slices can be composed into multi-dimensional data
    // structures. The length of the inner slices can
    // vary, unlike with multi-dimensional arrays.
    twoD := make([][]int, 3)
    for i := 0; i < 3; i++ {
        innerLen := i + 1
        twoD[i] = make([]int, innerLen)
        for j := 0; j < innerLen; j++ {
            twoD[i][j] = i + j
        }
    }
    fmt.Println("2d: ", twoD)
}
