package main

import "time"
import "math/rand"
import "fmt"

func f(n int) {
    for i := 0; i < 10; i++ {
        fmt.Println(n, ":", i)
        breakTime := time.Duration(rand.Intn(150))
        time.Sleep(time.Millisecond * breakTime)
    }
}

func main() {
    for i := 0; i < 5; i++ {
        f(i)
    }
    fmt.Println()
    for i := 0; i < 5; i++ {
        go f(i)
    }
    var input string
    fmt.Scanln(&input)
}
