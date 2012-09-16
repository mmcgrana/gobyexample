package main

import "fmt"

func main() {
    defer func() {    
        fmt.Println("Preparing for trouble...\n")
    }()
    panic("Trouble!")
}
