package main

import "fmt"
import "time"

func printer(done chan<- bool) {
    for i := 0; i < 10; i++ {
		time.Sleep(time.Millisecond * 100)
        fmt.Println(i)
    }
	done <- true
}

func main() {
	done := make(chan bool, 1)
    go printer(done)
	<- done
}
