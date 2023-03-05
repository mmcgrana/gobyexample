// `for` é a única ferramenta de repetição em Go.
// Aqui estão algumas formas básicas de utilização.

package main

import "fmt"

func main() {

	// O tipo mais simples com apenas uma condição.
	i := 1
	for i <= 3 {
		fmt.Println(i)
		i = i + 1
	}

	// O tipo clássico com inicial, condição
	// de continuação e pós iteração.
	for j := 7; j <= 9; j++ {
		fmt.Println(j)
	}

	// `for` sem nenhuma condição será repetido indefinidamente,
	// até que `break` ou `return` sejam usados para interromper.
	for {
		fmt.Println("loop")
		break
	}

	// Também é possível utilizar o comando `continue`
	// para prosseguir para a próxima iteração do loop.
	for n := 0; n <= 5; n++ {
		if n%2 == 0 {
			continue
		}
		fmt.Println(n)
	}
}
