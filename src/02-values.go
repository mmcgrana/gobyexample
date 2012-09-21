                                        // Go has various value types, including strings,
                                        // different types of numbers, booleans, etc.

package main

import "fmt"

func main() {
    fmt.Println("Hello world")          // Strings - you already saw these.
    fmt.Println("Hello World"[1])
    fmt.Println("Hello " + "World")

    fmt.Println("1+1 =", 1+1)           // Integers and floats.
    fmt.Println("7.0/3.0 =", 7.0/3.0)

    fmt.Println(true && false)          // Booleans as you'd expect.
    fmt.Println(true || false)
    fmt.Println(!true)
}

                                         // This is just a sampling of Go's value types. We'll
                                         // learn more about them as we go.
