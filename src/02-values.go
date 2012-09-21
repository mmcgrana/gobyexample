                                       // Go has various value types, including strings,
                                       // different types of numbers, booleans, etc.

package main

import "fmt"

func main() {
    fmt.Println("Hello world")         // Strings.
    fmt.Println("Hello " + "other")

    fmt.Println("1+1 =", 1+1)          // Integers and floats.
    fmt.Println("7.0/3.0 =", 7.0/3.0)

    fmt.Println(true && false)         // Booleans.
    fmt.Println(true || false)
    fmt.Println(!true)
}

/*
$ go run 02-values.go
Hello world
Hello other
1+1 = 2
7.0/3.0 = 2.3333333333333335
false
true
false
*/
