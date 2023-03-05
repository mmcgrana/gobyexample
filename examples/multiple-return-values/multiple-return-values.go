// Go tem suporte nativo para _múltiplos valores de retorno_.
// Esse recurso é utilizado frequentemente
// em Go idiomático, por exemplo, para retornar
// valores de resultado e de erro de uma função.

package main

import "fmt"

// A expressão `(int, int)` na assinatura desta função
// demonstra que a função retorna dois inteiros `int`.
func vals() (int, int) {
	return 3, 7
}

func main() {

	// Aqui são utilizados ambos valores retornados
	// da função com _atribuição múltipla_.
	a, b := vals()
	fmt.Println(a)
	fmt.Println(b)

	// Para utilizar apenas um dos valores retornados,
	// utiliza-se o identificador vazio `_`.
	_, c := vals()
	fmt.Println(c)
}
