// Общим требованием в программах является получение
// количества секунд, миллисекунд или наносекунд в [Unixtime](http://en.wikipedia.org/wiki/Unix_time).
// Вот как это сделать в Go.

package main

import (
	"fmt"
	"time"
)

func main() {

	// Используйте `time.Now` с `Unix` или `UnixNano`,
	// чтобы получить время, прошедшее с начала эпохи Unix в
	// секундах или наносекундах соответственно.
	now := time.Now()
	secs := now.Unix()
	nanos := now.UnixNano()
	fmt.Println(now)

	// Обратите внимание, что `UnixMillis` не существует,
	// поэтому, чтобы получить миллисекунды с начала эпохи Unix,
	// вам нужно будет вручную делить наносекунды.
	millis := nanos / 1000000
	fmt.Println(secs)
	fmt.Println(millis)
	fmt.Println(nanos)

	// Вы также можете конвертировать целые секунды или наносекунды
	// Unixtime в соответствующее `время`.
	fmt.Println(time.Unix(secs, 0))
	fmt.Println(time.Unix(0, nanos))
}
