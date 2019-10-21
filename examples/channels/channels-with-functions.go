//In this example, we give a boolean value into a channel by calculating an another channel
//If the integer channel(the value is random) is divided into two, the value of boolean channel will be false
//otherwise it will be true
package main

import (
	"fmt"
	"math/rand"
)

func channel_examples(number chan int, state chan bool) {

	number_value := <-number
	state_value := <-state

	if number_value%2 == 0 { //if the number is divided into two
		state_value = false
	}

	fmt.Println(number_value, state_value)

}

func main() {

	number := make(chan int)
	state := make(chan bool)

	go func() {
		number <- rand.Intn(10)
		state <- true
	}()
	channel_examples(number, state)

}
