package main

import "time"

func main() {
    jobs := make(chan int, 100)
    acks := make(chan bool, 100)

    for w := 0; w < 10; w++ {
        go func() {
            for j := range jobs {
                println("worker", w, "processing job", j)
                time.Sleep(time.Millisecond * 150)
                acks <- true
            }
        }()
    }

    for j := 0; j < 100; j++ {
        jobs <- j
    }

    for a := 0; a < 100; a++ {
        <-acks
    }
    println("all done")
}

// todo: broken
