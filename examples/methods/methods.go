// Go поддерживает _методы_ для структур

package main

import "fmt"

type rect struct {
	width, height int
}

// Метод `area` принимает _получателя_ `*rect`.
func (r *rect) area() int {
	return r.width * r.height
}

// Методы могут принимать как указатели, так и значения.
// Вот пример со значением.
func (r rect) perim() int {
	return 2*r.width + 2*r.height
}

func main() {
	r := rect{width: 10, height: 5}

	// Вызываем 2 метода, определенные для нашей структуры.
	fmt.Println("area: ", r.area())
	fmt.Println("perim:", r.perim())

	// Go автоматически обрабатывает преобразование между
	// значениями и указателями при вызове методов.
	// Возможно, вы захотите использовать указатель в
	// качестве получателя, чтобы избежать копирования при вызове
	// метода или позволить методу изменять структуру
	// получателя.
	rp := &r
	fmt.Println("area: ", rp.area())
	fmt.Println("perim:", rp.perim())
}
