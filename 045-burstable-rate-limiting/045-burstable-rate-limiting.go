package main

import "time"
import "fmt"

func main() {
	tick := time.Tick(time.Millisecond * 200)
	throttle := make(chan bool, 10)
	go func() {
		for {
			<- tick
			select {
			case throttle <- true:
			default: 
			}
		}
	}()
	time.Sleep(time.Millisecond * 1000)
	for {
  		<- throttle
  		go fmt.Println("acting")
    }
}

// todo: credit http://code.google.com/p/go-wiki/wiki/RateLimiting
