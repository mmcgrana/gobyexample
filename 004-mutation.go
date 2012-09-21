package main

import "fmt"

func main() {
    var x string
    x = "first"
    fmt.Println(x)
    x = "second"    // You can mutate variables in Go.
    fmt.Println(x)
}

/*
$ go run mutation.go
first
second
*/
