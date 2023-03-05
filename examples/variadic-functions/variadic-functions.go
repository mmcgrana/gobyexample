// [_Funções variádicas_](https://en.wikipedia.org/wiki/Variadic_function)
// podem ser chamads com qualquer número de argumentos.
// Por exemplo, `fmt.Println` é uma função variádica
// comumente utilizada.

package main

import "fmt"

// Aqui está uma função que aceitará um número arbitrário de
// inteiros `int`s como argumento(s).
// Atenção para o operador de espalhamento (spread operator)
// que deve preceder a declaração do tipo.
func sum(nums ...int) {
	fmt.Print(nums, " ")
	total := 0
	// Dentro da função, o tipo `nums` é
	// equivalente a `[]int`. É possível usar `len(nums)`,
	// iterar utilizando `range`, etc.
	for _, num := range nums {
		total += num
	}
	fmt.Println(total)
}

func main() {

	// Funções variádicas pode ser chamada de
	// forma usual com argumentos individuais.
	sum(1, 2)
	sum(1, 2, 3)

	// Se uma slice com multiplos argumentos estiver
	// disponível, é possível passá-la como parâmetro
	// para uma função variádica usando `func(slice...)`.
	// Atenção que agora o operador de espalhamento deve
	// suceder o nome do parâmetro.
	nums := []int{1, 2, 3, 4}
	sum(nums...)
}
