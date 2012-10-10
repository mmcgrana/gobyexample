package main

import "time"
import "fmt"

func main() {
    throttle := time.Tick(time.Millisecond * 200)
    for {
        <-throttle
        go fmt.Println("rate-limited action")
    }
}

// todo: credit code.google.com/p/go-wiki/wiki/RateLimiting
