// ## Timers

package main

import "time"
import "fmt"

func main() {
    timer1 := time.NewTimer(time.Second)
    <-timer1.C
    fmt.Println("Timer 1 expired")
    stop1 := timer1.Stop()
    fmt.Println("Timer 2 stopped:", stop1)

    timer2 := time.NewTimer(time.Second)
    go func() {
        <-timer2.C
        fmt.Println("Timer 2 expired")
    }()
    stop2 := timer2.Stop()
    fmt.Println("Timer 2 stopped:", stop2)
}
