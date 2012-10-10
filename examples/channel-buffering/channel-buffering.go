package main

import "fmt"

func main() {
    var messages chan string = make(chan string, 1)
    messages <- "ping"
    msg := <-messages
    fmt.Println(msg)
}
