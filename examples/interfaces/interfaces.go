// _Интерфейсы_ это коллекции методов.

package main

import (
	"fmt"
	"math"
)

// Пример базового интерфейса в Go
type geometry interface {
	area() float64
	perim() float64
}

// В нашем примере мы будем реализовывать этот интерфейс
// для типов `rect` и `circle`.
type rect struct {
	width, height float64
}
type circle struct {
	radius float64
}

// Чтобы реализовать интерфейс на Go, нам просто нужно
// реализовать все методы в интерфейсе. Здесь мы
// реализуем интерфейс `geometry` для `rect`.
func (r rect) area() float64 {
	return r.width * r.height
}
func (r rect) perim() float64 {
	return 2*r.width + 2*r.height
}

// Реализация для `circle`.
func (c circle) area() float64 {
	return math.Pi * c.radius * c.radius
}
func (c circle) perim() float64 {
	return 2 * math.Pi * c.radius
}

// Если переменная реализует интерфейс, то мы можем
// вызывать методы, которые находятся в этом интерфейсе.
// Функция `measure` использует это преимущество, для
// работы с любой фигурой.
func measure(g geometry) {
	fmt.Println(g)
	fmt.Println(g.area())
	fmt.Println(g.perim())
}

func main() {
	r := rect{width: 3, height: 4}
	c := circle{radius: 5}

	// Типы `circle` и `rect` структур реализуют
	// интерфейс `geometry`, поэтому мы можем использовать
	// экземпляры этих структур в качестве аргументов для
	// `measure`.
	measure(r)
	measure(c)
}
