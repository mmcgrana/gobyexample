// <em>[Ограничение скорости](http://en.wikipedia.org/wiki/Rate_limiting)</em>
// является важным механизмом контроля использования ресурсов и
// поддержания качества обслуживания. Go элегантно поддерживает
// ограничение скорости с помощью горутин, каналов и [тикеров](tickers).

package main

import (
	"fmt"
	"time"
)

func main() {

	// Сначала мы рассмотрим базовое ограничение скорости.
	// Предположим, мы хотим ограничить нашу обработку
	// входящих запросов. Мы будем обслуживать эти запросы
	// с одноименного канала.
	requests := make(chan int, 5)
	for i := 1; i <= 5; i++ {
		requests <- i
	}
	close(requests)

	// Канал `limiter` будет получать значение каждые
	// 200мс. Это то, что регулирует скорость в нашей
	// схеме.
	limiter := time.Tick(200 * time.Millisecond)

	// Блокируя прием от канала `limiter` перед
	// обслуживанием каждого запроса, мы ограничиваем себя
	// одним запросом каждые 200 миллисекунд.
	for req := range requests {
		<-limiter
		fmt.Println("request", req, time.Now())
	}

	// Мы можем разрешить короткие всплески запросов в
	// нашей схеме ограничения скорости при сохранении
	// общего ограничения скорости. Мы можем сделать это
	// путем буферизации нашего канала ограничения. Этот
	// канал `burstyLimiter` будет позволять делать до
	// 3 событий.
	burstyLimiter := make(chan time.Time, 3)

	// Заполняем канал, чтобы предоставить возможность
	// ускорить.
	for i := 0; i < 3; i++ {
		burstyLimiter <- time.Now()
	}

	// Каждые 200мс мы будем пытаться добавлять новое
	// значение в `burstyLimiter`, до своего предела
	// в 3 значения.
	go func() {
		for t := range time.Tick(200 * time.Millisecond) {
			burstyLimiter <- t
		}
	}()

	// Теперь смоделируем еще 5 входящих запросов. Первые
	// 3 из них получат выгоду от вместимости `burstyLimiter`.
	burstyRequests := make(chan int, 5)
	for i := 1; i <= 5; i++ {
		burstyRequests <- i
	}
	close(burstyRequests)
	for req := range burstyRequests {
		<-burstyLimiter
		fmt.Println("request", req, time.Now())
	}
}
