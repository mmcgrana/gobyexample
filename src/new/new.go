package main

import "fmt"

func one(xPtr *int) {
    *xPtr = 1
}
func main() {
    xPtr := new(int)
    fmt.Println(xPtr)
    fmt.Println(*xPtr)
    one(xPtr)
    fmt.Println(xPtr)
    fmt.Println(*xPtr)
}
