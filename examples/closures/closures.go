package main

import "fmt"

func makeEvenGenerator() func() uint {
    i := uint(0)
    return func() uint {
        i += 2
        return i
    }
}

func main() {
    nextEven := makeEvenGenerator()
    fmt.Println(nextEven())
    fmt.Println(nextEven())
    fmt.Println(nextEven())
}

// todo: note example of first-class functions.
