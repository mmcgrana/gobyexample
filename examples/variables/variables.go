// Em Go, _variáveis_ são explicitamente declaradas
// e usadas pelo compilador para, por exemplo,
// verificar validade de tipos em chamadas a funções.

package main

import "fmt"

func main() {

	// `var` é uma palavra reservada que é utilizada
	// para declarar variáveis.
	var a = "initial"
	fmt.Println(a)

	// Você pode declarar mais de uma variável.
	var b, c int = 1, 2
	fmt.Println(b, c)

	// Go, na ausência de declaração de um tipo, irá inferir
	// o tipo da variável inicializada.
	var d = true
	fmt.Println(d)

	// Variáveis declaradas sem um tipo correspondente são
	// inicializadas com valores padrões, ou zero (zero-value).
	// Por exemplo, o valor padrão para uma variável do tipo
	// `int` é `0`.
	var e int
	fmt.Println(e)

	// A sintaxe `:=` é uma abreviação para declarar e
	// inicializar uma variavel. Por exemplo,
	// `var f string = "apple"`.
	// Esta sintaxe é permitida somente dentro de funções.
	f := "apple"
	fmt.Println(f)
}
