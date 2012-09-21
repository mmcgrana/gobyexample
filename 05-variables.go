package main

import "fmt"

func main() {
    var x string = "Hello world"  // `var` declares 1 or more variables. The type comes
    fmt.Println(x)                // at the end.

    var a, b int = 1, 2           // An example of declaring multiple `int` variables.
    fmt.Println(a, b)
}

/*
$ go run variables.go
Hello world
1 2
*/
