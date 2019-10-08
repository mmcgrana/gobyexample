// Нам часто нужны наши программы для выполнения операций
// с коллекциями данных, таких как выбор всех элементов,
// которые удовлетворяют данному предикату, или отображение
// всех элементов в новую коллекцию с помощью
// пользовательской функции.

// В некоторых языках идиоматично использовать [дженерики](http://en.wikipedia.org/wiki/Generic_programming)
// и алгоритмы. Go не поддерживает дженерики; в Go, как
// правило, предоставляют функции коллекции если они
// необходимы конкретно для вашей программы и ваших
// типов данных.

// Вот несколько примеров функций коллекции для срезов
// со `строковыми значениями`. Вы можете использовать эти
// примеры, чтобы сделать собственные функции. Обратите
// внимание, что в некоторых случаях, возможно, было бы
// более явным встроить код, манипулирующий коллекциями,
// вместо создания создания вспомогательных функций.

package main

import (
	"fmt"
	"strings"
)

// Возвращает первый индекс совпадения со строкой `t` или -1
// если совпадение не найдено.
func Index(vs []string, t string) int {
	for i, v := range vs {
		if v == t {
			return i
		}
	}
	return -1
}

// Возвращает `true` если строка `t` находится в срезе.
func Include(vs []string, t string) bool {
	return Index(vs, t) >= 0
}

// Возвращает `true` если одна из строк в срезе
// удовлетворяет условию `f`.
func Any(vs []string, f func(string) bool) bool {
	for _, v := range vs {
		if f(v) {
			return true
		}
	}
	return false
}

// Возвращает `true` если все из строк в срезе
// удовлетворяют условие `f`.
func All(vs []string, f func(string) bool) bool {
	for _, v := range vs {
		if !f(v) {
			return false
		}
	}
	return true
}

// Возвращает новый срез, содержащий все строки в срезе,
// которые удовлетворяют условие `f`.
func Filter(vs []string, f func(string) bool) []string {
	vsf := make([]string, 0)
	for _, v := range vs {
		if f(v) {
			vsf = append(vsf, v)
		}
	}
	return vsf
}

// Возвращает новый срез, содержащий результаты выполнения
// функции `f` с каждой строкой исходного слайса.
func Map(vs []string, f func(string) string) []string {
	vsm := make([]string, len(vs))
	for i, v := range vs {
		vsm[i] = f(v)
	}
	return vsm
}

func main() {

	// Здесь мы пробуем различные функции коллекций.
	var strs = []string{"peach", "apple", "pear", "plum"}

	fmt.Println(Index(strs, "pear"))

	fmt.Println(Include(strs, "grape"))

	fmt.Println(Any(strs, func(v string) bool {
		return strings.HasPrefix(v, "p")
	}))

	fmt.Println(All(strs, func(v string) bool {
		return strings.HasPrefix(v, "p")
	}))

	fmt.Println(Filter(strs, func(v string) bool {
		return strings.Contains(v, "e")
	}))

	// Все примеры, приведенные выше, использовали
	// анонимные функции, но вы можете использовать именные
	// функции корректного типа.
	fmt.Println(Map(strs, strings.ToUpper))

}
