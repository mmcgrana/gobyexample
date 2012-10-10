package main

import "fmt"

func pinger(pings chan<- string) {
    for i := 0; i <= 10; i++ {
        pings <- "ping"
    }
}

func ponger(pings <-chan string, pongs chan<- string) {
    for {
        <-pings
        pongs <- "pong"
    }
}

func printer(pongs <-chan string) {
    for {
        msg := <-pongs
        fmt.Println(msg)
    }
}

func main() {
    var pings chan string = make(chan string)
    var pongs chan string = make(chan string)

    go pinger(pings)
    go ponger(pings, pongs)
    go printer(pongs)

    var input string
    fmt.Scanln(&input)
}
