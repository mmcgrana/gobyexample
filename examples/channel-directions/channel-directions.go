// When using channels as function parameters, you can
// specify if a channel is meant to only send or receive
// values. This specificity increases the type-safety of
// the program.

package main

import "fmt"

// This `send` function only accepts a channel for sending
// values. It would be a compile-time error to try to
// receive on this channel.
func send(sendCh chan<- string, msg string) {
	fmt.Println("send message:", msg)
	sendCh <- msg
}

// The `fwd` function accepts one channel for receives
// (`sendCh`) and a second for sends (`forwardCh`).
func fwd(sendCh <-chan string, forwardCh chan<- string) {
	msg := <-sendCh
	fmt.Println("forwarding:", msg)
	forwardCh <- msg
}

func main() {
	sendCh := make(chan string, 1)
	forwardCh := make(chan string, 1)
	send(sendCh, "Hello, World!")
	fwd(sendCh, forwardCh)
	fmt.Println("reading messages: ", <-forwardCh)
}
