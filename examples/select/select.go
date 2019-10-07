// Go's _select позволяет вам ждать нескольких
// операций на канале. Сочетание горутин и каналов
// с помощью select'а - мощная функция Go.

package main

import (
	"fmt"
	"time"
)

func main() {

	// В нашем примере мы будем выбирать между двумя
	// каналами.
	c1 := make(chan string)
	c2 := make(chan string)

	// Каждый канал получит значение через некоторое время,
	// например, для моделирования блокировки RPC-операций,
	// выполняемых в параллельных горутинах.
	go func() {
		time.Sleep(1 * time.Second)
		c1 <- "one"
	}()
	go func() {
		time.Sleep(2 * time.Second)
		c2 <- "two"
	}()

	// Мы будем использовать `select`, чтобы ожидать
	// оба значения одновременно, печатая каждое из
	// них по мере поступления.
	for i := 0; i < 2; i++ {
		select {
		case msg1 := <-c1:
			fmt.Println("received", msg1)
		case msg2 := <-c2:
			fmt.Println("received", msg2)
		}
	}
}
