// В предыдущем примере мы использовали явную блокировку
// с [мьютексами](mutexes) для синхронизации доступа к
// общему состоянию между несколькими горутинами. Другой
// вариант - использовать встроенные функции синхронизации
// для горутин и каналов для достижения того же результата.
// Этот подход, основанный на каналах, согласуется с идеями
// Go о совместном использовании памяти путем обмена
// данными и владения каждой частью данных ровно 1 горутиной.

package main

import (
	"fmt"
	"math/rand"
	"sync/atomic"
	"time"
)

// В этом примере наше состояние будет принадлежать
// единственной горутине. Это гарантирует, что данные
// никогда не будут повреждены при одновременном доступе.
// Чтобы прочитать или записать это состояние, другие
// горутины будут отправлять сообщения горутин-владельцу
// и получать соответствующие ответы. Эти `структуры`
// `readOp` и `writeOp` инкапсулируют эти запросы и
// способ, которым владеет горутина-ответчик.
// In this example our state will be owned by a single
// goroutine. This will guarantee that the data is never
// corrupted with concurrent access. In order to read or
// write that state, other goroutines will send messages
// to the owning goroutine and receive corresponding
// replies. These `readOp` and `writeOp` `struct`s
// encapsulate those requests and a way for the owning
// goroutine to respond.
type readOp struct {
	key  int
	resp chan int
}
type writeOp struct {
	key  int
	val  int
	resp chan bool
}

func main() {

	// Как и прежде, мы посчитаем, сколько операций мы
	// выполняем.
	var readOps uint64
	var writeOps uint64

	// Каналы `чтения` и `записи` будут использоваться
	// другими горутинами для выдачи запросов на чтение
	// и запись соответственно.
	reads := make(chan readOp)
	writes := make(chan writeOp)

	// Эта горутина, которой принадлежит состояние, она же
	// является картой, как в предыдущем примере, но теперь
	// является частной для горутины с сохранением состояния.
	// Она постоянно выбирает каналы `чтения` и `записи`,
	// отвечая на запросы по мере их поступления. Ответ
	// выполняется, сначала выполняя запрошенную операцию,
	// а затем отправляя значение по каналу `resp`,
	// соответственно, чтобы указать успешность (и
	// ребуемое значение в случае `reads`).
	go func() {
		var state = make(map[int]int)
		for {
			select {
			case read := <-reads:
				read.resp <- state[read.key]
			case write := <-writes:
				state[write.key] = write.val
				write.resp <- true
			}
		}
	}()

	// Запускаем 100 горутин для выдачи операций чтения
	// в горутину владеющую состоянием, через канал `reads`.
	// Каждое чтение требует создания `readOp`, отправки
	// его по каналу `reads` и получения результата по
	// `resp` каналу.
	for r := 0; r < 100; r++ {
		go func() {
			for {
				read := readOp{
					key:  rand.Intn(5),
					resp: make(chan int)}
				reads <- read
				<-read.resp
				atomic.AddUint64(&readOps, 1)
				time.Sleep(time.Millisecond)
			}
		}()
	}

	// Так же делаем 10 записей.
	for w := 0; w < 10; w++ {
		go func() {
			for {
				write := writeOp{
					key:  rand.Intn(5),
					val:  rand.Intn(100),
					resp: make(chan bool)}
				writes <- write
				<-write.resp
				atomic.AddUint64(&writeOps, 1)
				time.Sleep(time.Millisecond)
			}
		}()
	}

	// Дадим горутинам отработать 1 секунду
	time.Sleep(time.Second)

	// Наконец, выводим данные счетчиков
	readOpsFinal := atomic.LoadUint64(&readOps)
	fmt.Println("readOps:", readOpsFinal)
	writeOpsFinal := atomic.LoadUint64(&writeOps)
	fmt.Println("writeOps:", writeOpsFinal)
}
