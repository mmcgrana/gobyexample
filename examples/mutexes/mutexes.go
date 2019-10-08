// В предыдущем примере мы увидели, как управлять простым
// состоянием счетчика с помощью [атомарных операций](atomic-counters).
// Для более сложного состояния мы можем использовать <em>мьютекс(http://en.wikipedia.org/wiki/Mutual_exclusion)</em>
// для безопасного доступа к данным в нескольких горутинах.

package main

import (
	"fmt"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

func main() {

	// Для нашего примера `state` будет картой.
	var state = make(map[int]int)

	// Этот `mutex` будет синхронизировать доступ к `state`.
	var mutex = &sync.Mutex{}

	// Мы будем отслеживать, сколько операций чтения и
	// записи мы выполняем.
	var readOps uint64
	var writeOps uint64

	// Здесь мы запускаем 100 горутин для выполнения
	// повторных операций чтения по состоянию, один раз
	// в миллисекунду в каждой горутине.
	for r := 0; r < 100; r++ {
		go func() {
			total := 0
			for {

				// Для каждого чтения мы выбираем ключ для
				// доступа, блокируем `mutex` с помощью `Lock()` ,
				// чтобы обеспечить исключительный доступ к
				// `состоянию`, читаем значение в выбранном ключе,
				// разблокируем мьютекс `Unlock()` и увеличиваем
				// количество `readOps`.
				key := rand.Intn(5)
				mutex.Lock()
				total += state[key]
				mutex.Unlock()
				atomic.AddUint64(&readOps, 1)

				// Немного ждем между чтениями.
				time.Sleep(time.Millisecond)
			}
		}()
	}

	// Запустим так же 10 горутин для симуляции записи,
	// так же как мы делали для чтения.
	for w := 0; w < 10; w++ {
		go func() {
			for {
				key := rand.Intn(5)
				val := rand.Intn(100)
				mutex.Lock()
				state[key] = val
				mutex.Unlock()
				atomic.AddUint64(&writeOps, 1)
				time.Sleep(time.Millisecond)
			}
		}()
	}

	// Пусть 10 горутин работают над `состоянием` и
	// `мьютексом` на секунду.
	time.Sleep(time.Second)

	// Смотрим финальное количество операций
	readOpsFinal := atomic.LoadUint64(&readOps)
	fmt.Println("readOps:", readOpsFinal)
	writeOpsFinal := atomic.LoadUint64(&writeOps)
	fmt.Println("writeOps:", writeOpsFinal)

	// С окончательной блокировкой `состояния` смотрим,
	// как все закончилось.
	mutex.Lock()
	fmt.Println("state:", state)
	mutex.Unlock()
}
