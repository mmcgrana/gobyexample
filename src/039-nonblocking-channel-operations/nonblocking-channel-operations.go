package main

import "fmt"

func main() {
    messages := make(chan string)
    signals := make(chan bool)

    // Non-blocking receive.
    select {
    case msg := <-messages:
        fmt.Println("received message", msg)
    default:
        fmt.Println("no messages received")
    }

    // Non-blocking send.
    msg := "hi"
    select {
    case messages <- msg:
        fmt.Println("sent message", msg)
    default:
        fmt.Println("no messages sent")
    }

    // Non-blocking multi-way select.
    select {
    case msg := <-messages:
        fmt.Println("received message", msg)
    case sig := <-signals:
        fmt.Println("received signal", sig)
    default:
        fmt.Println("no activity")
    }
}
