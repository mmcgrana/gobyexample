// Пакет `filepath` предоставляет функции для разбора и
// создания *путей к файлам* способом, который переносим
// между операционными системами; например, `dir/file` в
// Linux против `dir\file` в Windows.
package main

import (
	"fmt"
	"path/filepath"
	"strings"
)

func main() {

	// `Join` должен использоваться для создания путей в
	// переносимом виде. Он принимает любое количество
	// аргументов и строит из них иерархический путь.
	p := filepath.Join("dir1", "dir2", "filename")
	fmt.Println("p:", p)

	// Вы должны всегда использовать `Join` вместо ручного
	// объединения с помощью слешей `/` или `\`. В дополнение
	// к обеспечению переносимости, `Join` также нормализует
	// пути, удаляя лишние разделители.
	fmt.Println(filepath.Join("dir1//", "filename"))
	fmt.Println(filepath.Join("dir1/../dir1", "filename"))

	// `Dir` и `Base` могут использоваться для разделения
	// пути к каталогу и файлу. В качестве альтернативы,
	// `Split` вернет оба в одном вызове.
	fmt.Println("Dir(p):", filepath.Dir(p))
	fmt.Println("Base(p):", filepath.Base(p))

	// Можно проверить является ли путь абсолютным.
	fmt.Println(filepath.IsAbs("dir/file"))
	fmt.Println(filepath.IsAbs("/dir/file"))

	filename := "config.json"

	// Некоторые имена файлов имеют расширения, следующие
	// за точкой. Мы можем получить расширение из таких
	// имен с помощью `Ext`.
	ext := filepath.Ext(filename)
	fmt.Println(ext)

	// Чтобы найти имя файла с удаленным расширением,
	// используйте `strings.TrimSuffix`.
	fmt.Println(strings.TrimSuffix(filename, ext))

	// `Rel` находит относительный путь между двумя путями
	// *base* и *target*. Возвращает ошибку, если *target*
	// не может быть получен из *base*.
	rel, err := filepath.Rel("a/b", "a/b/t/file")
	if err != nil {
		panic(err)
	}
	fmt.Println(rel)

	rel, err = filepath.Rel("a/b", "a/c/t/file")
	if err != nil {
		panic(err)
	}
	fmt.Println(rel)
}
