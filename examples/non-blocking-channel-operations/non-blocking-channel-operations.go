// Стандартные отправители и получатели в каналах
// являются блокирующими.Тем не менее, мы можем
// использовать `select` с выбором `по умолчанию` для
// реализации _неблокирующих_ отправителей, получателей
// и даже неблокирующего множественного `select`'а.

package main

import "fmt"

func main() {
	messages := make(chan string)
	signals := make(chan bool)

	// Вот неблокирующее получение. Если значение
	// доступно в `messages`, тогда `select` выберет
	// `<-messages` с этим значением. Если нет, он
	// сразу же примет случай `по-умолчанию`.
	select {
	case msg := <-messages:
		fmt.Println("received message", msg)
	default:
		fmt.Println("no message received")
	}

	// Неблокирующая отправка работает аналогично.
	// Здесь `msg` не может быть отправлено в канал
	// `messages`, потому что у канала нет буфера и
	// нет получателя. Поэтому выбирается действие
	// `по-умолчанию`.
	msg := "hi"
	select {
	case messages <- msg:
		fmt.Println("sent message", msg)
	default:
		fmt.Println("no message sent")
	}

	// Мы можем использовать несколько `case`'ов перед
	// `действием по-умолчанию` для реализации
	// многоцелевого неблокирующего выбора. Здесь мы
	// пытаемся получить неблокирующее получение
	// `messages` и `signals`.
	select {
	case msg := <-messages:
		fmt.Println("received message", msg)
	case sig := <-signals:
		fmt.Println("received signal", sig)
	default:
		fmt.Println("no activity")
	}
}
