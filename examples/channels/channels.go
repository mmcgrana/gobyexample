package main

import "fmt"

func main() {
    var messages chan string = make(chan string)
    go func() { messages <- "ping" }()
    msg := <-messages
    fmt.Println(msg)
}
