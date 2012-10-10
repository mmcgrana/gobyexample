// Use `os.Args` to access command-line arguments and
// the name of the program.
package main

import "os"
import "fmt"

func main() {
    // `os.Args` includes the program name as the first
    // value.
    argsWithProg := os.Args
    argsWithoutProg := os.Args[1:]

    // `Args` are a slice, you can get individual args
    // with normal indexing.
    arg := os.Args[3]

    fmt.Println(argsWithProg)
    fmt.Println(argsWithoutProg)
    fmt.Println(arg)
}

// todo: discuss building before here
