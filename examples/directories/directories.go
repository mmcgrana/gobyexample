// Go имеет несколько полезных функций для работы
// с *каталогами* в файловой системе.

package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {

	// Создадим новую суб-директорию в текущей рабочей
	// папке.
	err := os.Mkdir("subdir", 0755)
	check(err)

	// Когда мы создаем временную директорию, хорошим
	// тоном является удалить ее через `defer`.
	// `os.RemoveAll` удалит директорию и все, что в ней
	// находится (по аналогии с `rm -rf`).
	defer os.RemoveAll("subdir")

	// Функция-помощник для создания нового пустого файла.
	createEmptyFile := func(name string) {
		d := []byte("")
		check(ioutil.WriteFile(name, d, 0644))
	}

	createEmptyFile("subdir/file1")

	// Мы можем создать иерархию из директорий, включая все
	// родительские, с помощью `MkdirAll`. Это является аналогом
	// команды `mkdir -p`.
	err = os.MkdirAll("subdir/parent/child", 0755)
	check(err)

	createEmptyFile("subdir/parent/file2")
	createEmptyFile("subdir/parent/file3")
	createEmptyFile("subdir/parent/child/file4")

	// `ReadDir` перечисляет содержимое каталогов,
	// возвращая срез объектов `os.FileInfo`.
	c, err := ioutil.ReadDir("subdir/parent")
	check(err)

	fmt.Println("Listing subdir/parent")
	for _, entry := range c {
		fmt.Println(" ", entry.Name(), entry.IsDir())
	}

	// `Chdir` позволяет изменить текущую рабочую
	// директорию, по аналогии с `cd`.
	err = os.Chdir("subdir/parent/child")
	check(err)

	// Теперь мы увидим содержимое директории
	// `subdir/parent/child`, когда запросим листинг
	// *текущей* директории.
	c, err = ioutil.ReadDir(".")
	check(err)

	fmt.Println("Listing subdir/parent/child")
	for _, entry := range c {
		fmt.Println(" ", entry.Name(), entry.IsDir())
	}

	// Вернемся в начало
	err = os.Chdir("../../..")
	check(err)

	// Мы также можем *рекурсивно* обойти каталог, включая
	// все его подкаталоги. `Walk` принимает функцию обратного
	// вызова для обработки каждого файла или каталога,
	// которые посетили.
	fmt.Println("Visiting subdir")
	err = filepath.Walk("subdir", visit)
}

// `visit` вызывается для каждого найденного элемента в `filepath.Walk`.
func visit(p string, info os.FileInfo, err error) error {
	if err != nil {
		return err
	}
	fmt.Println(" ", p, info.IsDir())
	return nil
}
