// Запись файло в Go следует тем же шаблонам, что мы
// видели ранее в главе "`Чтение`".

package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {

	// В этом примере показано вот как записать строку
	// (или только байты) в файл.
	d1 := []byte("hello\ngo\n")
	err := ioutil.WriteFile("/tmp/dat1", d1, 0644)
	check(err)

	// Для более детальной записи откройте файл для записи.
	f, err := os.Create("/tmp/dat2")
	check(err)

	// Идиоматично откладывать `закрытие` с помощью `defer`'a
	// сразу после открытия файла.
	defer f.Close()

	// Вы можете `записать` срез байт, как и ожидается.
	d2 := []byte{115, 111, 109, 101, 10}
	n2, err := f.Write(d2)
	check(err)
	fmt.Printf("wrote %d bytes\n", n2)

	// Запись строки `WriteString` так же доступна.
	n3, err := f.WriteString("writes\n")
	fmt.Printf("wrote %d bytes\n", n3)

	// Выполните синхронизацию `Sync` для сброса записей
	// в стабильное хранилище.
	f.Sync()

	// `bufio` предоставляет буферизованных `писателей`
	// в дополнение к буферизованным `читателям`, которые
	// мы видели ранее.
	w := bufio.NewWriter(f)
	n4, err := w.WriteString("buffered\n")
	fmt.Printf("wrote %d bytes\n", n4)

	// Используйте `Flush`, чтобы убедиться, что все
	// буферизованные операции были применены к основному
	// модулю записи.
	w.Flush()

}
