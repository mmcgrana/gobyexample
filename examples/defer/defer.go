package main

import "fmt"

func first() {
    fmt.Println("1st")
}

func second() {
    fmt.Println("2nd")
}

func main() {
    defer second()
    first()
}

// todo: review http://blog.golang.org/2010/08/defer-panic-and-recover.html
