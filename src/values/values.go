// Go has various value types, including strings,
// different types of numbers, booleans, etc.
package main

import "fmt"

func main() {

    // Here are some strings, which can be added together.
    fmt.Println("Hello world")
    fmt.Println("Hello " + "other")

    // Some examples of integers and floats.
    fmt.Println("1+1 =", 1+1)
    fmt.Println("7.0/3.0 =", 7.0/3.0)

    // And booleans, which work as you'd expect.
    fmt.Println(true && false)
    fmt.Println(true || false)
    fmt.Println(!true)
}
