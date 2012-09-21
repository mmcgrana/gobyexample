package main                           // Use the `os` package to list, set, and get
                                       // environment variables.
import (
    "os"
    "fmt"
    "strings"
)

func main() {
    for _, e := range os.Environ() {   // `Environ` returns a slice of strings in the form
        pair := strings.Split(e, "=")  // `KEY=value`. You can `strings.Split` them to get
        fmt.Println(pair[0])           // the key and value.
    }
    fmt.Println()

    os.Setenv("FOO", "bar")            // `Setenv` sets a given key, value pair.
    fmt.Println(os.Getenv("FOO"))      // `Getenv` returns the value at key, or an empty
}                                      // string if the key isn't present.

/*
$ go run env.go
HOME
PATH
PWD
...
bar
*/
