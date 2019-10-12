// [_Аргументы командной строки_](http://en.wikipedia.org/wiki/Command-line_interface#Arguments)
// являются распространенным способом параметризации
// выполнения программ. Например, `go run hello.go`
// использует аргументы `run` и `hello.go` для программы
// `go`.

package main

import (
	"fmt"
	"os"
)

func main() {

	// `os.Args` предоставляет доступ к необработанным
	// аргументам командной строки. Обратите внимание,
	// что первое значение в этом срезе - это путь к
	// программе, а os.Args [1:] содержит аргументы
	// программы.
	argsWithProg := os.Args
	argsWithoutProg := os.Args[1:]

	// Вы можете получить отдельные аргументы с
	// обычной индексацией.
	arg := os.Args[3]

	fmt.Println(argsWithProg)
	fmt.Println(argsWithoutProg)
	fmt.Println(arg)
}
