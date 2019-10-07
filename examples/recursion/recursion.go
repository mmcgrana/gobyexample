// Go поддерживает
// <a href="http://en.wikipedia.org/wiki/Recursion_(computer_science)"><em>рекурсивные функции</em></a>.
// Ниже приведено классическое вычисление факториала.

package main

import "fmt"

// Фукция `fact` вызывает себя по не достигнет
// `fact(0)`.
func fact(n int) int {
	if n == 0 {
		return 1
	}
	return n * fact(n-1)
}

func main() {
	fmt.Println(fact(7))
}
