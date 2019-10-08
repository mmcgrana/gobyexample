// _Defer_ используется, чтобы гарантировать, что
// вызов функции будет выполнен позже при выполнении
// программы, обычно для целей очистки. `defer` часто
// используется там, где в других языках используются
// `ensure` и `finally`.

package main

import (
	"fmt"
	"os"
)

// Предположим, мы хотим создать файл, записать в него,
// а затем закрыть, когда закончим. Вот как нам поможет
// `defer`.
func main() {

	// Сразу же после получения объекта файла с помощью
	// `createFile` мы откладываем закрытие этого файла
	// с помощью `closeFile`. Она будет выполнена в
	// конце включающей функции (`main`) после завершения
	// `writeFile`.
	f := createFile("/tmp/defer.txt")
	defer closeFile(f)
	writeFile(f)
}

func createFile(p string) *os.File {
	fmt.Println("creating")
	f, err := os.Create(p)
	if err != nil {
		panic(err)
	}
	return f
}

func writeFile(f *os.File) {
	fmt.Println("writing")
	fmt.Fprintln(f, "data")

}

func closeFile(f *os.File) {
	fmt.Println("closing")
	err := f.Close()
	// Важно проверять наличие ошибок при закрытии файла,
	// даже в отложенной функции.
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}
