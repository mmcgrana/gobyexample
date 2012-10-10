package main

// Use the `os` package to list, set, and get environment
// variables.
import "os"
import "strings"
import "fmt"

func main() {
    // `os.Environ` returns a slice of strings in the form
    // `KEY=value`. You can `strings.Split` them to get
    // the key and value.
    for _, e := range os.Environ() {
        pair := strings.Split(e, "=")
        fmt.Println(pair[0])
    }
    fmt.Println()

    // `Setenv` sets a given key, value pair.
    // `Getenv` returns the value at key, or an empty
    // string if the key isn't present.
    os.Setenv("FOO", "bar")
    fmt.Println(os.Getenv("FOO"))
}
