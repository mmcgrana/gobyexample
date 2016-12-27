// Go has built-in support for _multiple return values_.
// This feature is used often in idiomatic Go, for example
// to return both result and error values from a function.

package main

import "fmt"
import "os"

// The `(int, int)` in this function signature shows that
// the function returns 2 `int`s.
func vals() (int, int) {
    return 3, 7
}

// Return values can also be named.  This can be useful to
// help document the purpose of each return value.  These
// names can be referenced in the function body like any
// other variable.  A naked return will use the current
// value of these variables as the result.
func splitPrice(price float32) (dollars, cents int) {
    dollars = int(price)
    cents = int((price - float32(dollars)) * 100.0)
    return
}

// One use case for named return values is when you want
// to modify a return value in the function's defer
// statement.
func getFileSize() (file_size int64, had_error bool) {
    f, err := os.Open("/tmp/dat")
    if err != nil {
        return 0, true
    }
    defer func() {
        if err := f.Close(); err != nil {
            had_error = true
        }
    }()

    fi, err := f.Stat()
    if err != nil {
        return 0, true
    }
    return fi.Size(), false
}

func main() {

    // Here we use the 2 different return values from the
    // call with _multiple assignment_.
    a, b := vals()
    fmt.Println(a)
    fmt.Println(b)

    // If you only want a subset of the returned values,
    // use the blank identifier `_`.
    _, c := vals()
    fmt.Println(c)

    dollars, cents := splitPrice(12.42)
    fmt.Println(dollars, cents)

    file_size, had_error := getFileSize()
    fmt.Println(file_size, had_error)
}
