package main

import "time"
import "fmt"

func main() {
    c := make(chan string)
    d := make(chan bool, 1)

    go func() {
        time.Sleep(time.Millisecond * 1500)
        c <- "ready"
    }()

    go func() {
        select {
        case msg := <-c:
            fmt.Println(msg)
        case <-time.After(time.Millisecond * 1000):
            fmt.Println("timeout")
        }
        d <- true
    }()
    <-d
}
