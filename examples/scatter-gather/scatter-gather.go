package main

import "sync"
import "time"
import "math/rand"
import "fmt"

func main() {
    times := new([20]int)
    wait := new(sync.WaitGroup)
    for i := 0; i < 20; i++ {
        n := i
        wait.Add(1)
        go func() {
            opTime := time.Duration(rand.Intn(2000))
            time.Sleep(opTime * time.Millisecond)
            fmt.Println(n)
            times[n] = opTime
            wait.Done()
        }()
    }
    wait.Wait()
    fmt.Println(*times)
}
