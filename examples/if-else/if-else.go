// A condicional `if` e `else` em Go é bem direta.

package main

import "fmt"

func main() {

	// Aqui está um exemplo básico.
	if 7%2 == 0 {
		fmt.Println("7 é par")
	} else {
		fmt.Println("7 é ímpar")
	}

	// Também é possível utilizar o `if` sem `else`.
	if 8%4 == 0 {
		fmt.Println("8 é divisível por 4")
	}

	// Declarações podem preceder as condições; qualquer
	// variável declarada na estrutura condicional ficará
	// disponível em todas as suas ramificações.
	if num := 9; num < 0 {
		fmt.Println(num, "é negativo")
	} else if num < 10 {
		fmt.Println(num, "possui 1 dígito")
	} else {
		fmt.Println(num, "possui múltiplos dígitos")
	}
}

// É importante lembrar que não é necessário envelopar
// condicionais com parenteses em Go, no entanto,
// as chaves {} são necessárias.
