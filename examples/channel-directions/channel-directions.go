// При использовании каналов в качестве параметров
// функции вы можете указать, предназначен ли канал
// только для отправки или получения значений. Эта
// возможность повышает безопасность программы.

package main

import "fmt"

// Функция `ping` принимает канал только для отправки
// значений. При попытке получения значений в этот канал
// в процессе компиляции возниканет ошибка.
func ping(pings chan<- string, msg string) {
	pings <- msg
}

// Функция `pong` принимает один канал для приема
// (`pings`) и второй для отправки (`pongs`).
func pong(pings <-chan string, pongs chan<- string) {
	msg := <-pings
	pongs <- msg
}

func main() {
	pings := make(chan string, 1)
	pongs := make(chan string, 1)
	ping(pings, "passed message")
	pong(pings, pongs)
	fmt.Println(<-pongs)
}
