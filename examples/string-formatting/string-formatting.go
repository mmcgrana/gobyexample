package main

import "fmt"

type Point struct {
    x, y int
}

func main() {
    point := Point{1, 2}

    fmt.Printf("default: %v\n", point)
    fmt.Printf("default w/ vals: %+v\n", point)
    fmt.Printf("go: %#v\n", point)
    fmt.Printf("go type: %T\n", point)

    fmt.Printf("boolean: %t\n", true)
}

// todo: the rest
