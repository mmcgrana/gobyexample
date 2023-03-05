// _Functions_ são centrais em Go. Serão demonstradas
// funções com alguns exemplos diferentes.

package main

import "fmt"

// Aqui está uma função que recebe dois inteiros `int` e retorna a soma de ambos como outro inteiro `int`.
func plus(a int, b int) int {

	// Go exige retornos explícitos. Por exemplo,
	// não será retornado automaticamente o valor
	// da última expressão
	return a + b
}

// Ao existir multiplos parâmetros consecutivos de um
// mesmo tipo, é possível omitir o tipo dos parâmetros
// até a declaração do último parâmetro daquele tipo.
func plusPlus(a, b, c int) int {
	return a + b + c
}

func main() {

	// Para executar uma função é utilizada a
	// sintaxe `nomeDaFuncao(argumentos)`.
	res := plus(1, 2)
	fmt.Println("1+2 =", res)

	res = plusPlus(1, 2, 3)
	fmt.Println("1+2+3 =", res)
}
