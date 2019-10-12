// [Переменные среды (или переменные окружения)](http://en.wikipedia.org/wiki/Environment_variable)
// - это [универсальный механизм передачи информации о конфигурации
// в программы Unix](http://www.12factor.net/config). Давайте
// посмотрим, как устанавливать, получать и перечислять переменные
// среды.

package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {

	// Чтобы установить пару ключ/значение, используйте
	// `os.Setenv`. Чтобы получить значение для ключа,
	// используйте `os.Getenv`. Это вернет пустую строку,
	// если ключ не присутствует в среде.
	os.Setenv("FOO", "1")
	fmt.Println("FOO:", os.Getenv("FOO"))
	fmt.Println("BAR:", os.Getenv("BAR"))

	// Используйте `os.Environ` для вывода списка всех пар
	// ключ/значение в среде. Это возвращает спез строк в
	// формате `KEY=value`. Вы можете использовать `strings.Split`,
	// чтобы получить ключ и значение. Здесь мы печатаем
	// все ключи.
	fmt.Println()
	for _, e := range os.Environ() {
		pair := strings.Split(e, "=")
		fmt.Println(pair[0])
	}
}
