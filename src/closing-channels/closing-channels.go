package main

import "fmt"
import "time"

func main() {
    jobs := make(chan bool, 5)
    done := make(chan bool)

    go func() {
        for {
            _, more := <-jobs
            if more {
                fmt.Println("received job")
            } else {
                fmt.Println("received all")
                done <- true
                return
            }
        }
    }()

    for i := 0; i < 5; i++ {
        jobs <- false
        fmt.Println("sent job")
    }

    time.Sleep(100 * time.Millisecond)
    close(jobs)
    fmt.Println("sent all")

    <-done
}
