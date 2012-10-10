package main

import "time"
import "fmt"

func main() {
    now := time.Now()
    fmt.Println(now)

    then := time.Date(
        2009, 11, 17, 20, 34, 58, 651387237, time.UTC)
    fmt.Println(then)

    diff := now.Sub(then)
    fmt.Println(diff)
}

// todo: extract parts
// todo: add duration
// todo: check before / after
