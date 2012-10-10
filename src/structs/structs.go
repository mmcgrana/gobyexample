package main

import "fmt"

type Circle struct {
    x, y, r float64
}

func main() {
    cEmptyPtr := new(Circle)
    fmt.Println(cEmptyPtr)
    fmt.Println(*cEmptyPtr)

    cValue := Circle{x: 1, y: 2, r: 5}
    fmt.Println(&cValue)
    fmt.Println(cValue)

    cOrdered := Circle{1, 2, 5}
    fmt.Println(cOrdered)
}

// todo: add field access
