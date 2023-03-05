// Declarações _Switch_ são geralmente utilizadas
// para condicionais com muitas ramificações.

package main

import (
	"fmt"
	"time"
)

func main() {

	// Aqui está um `switch` básico.
	i := 2
	fmt.Print("Imprima ", i, " como ")
	switch i {
	case 1:
		fmt.Println("um")
	case 2:
		fmt.Println("dois")
	case 3:
		fmt.Println("três")
	}

	// Vírgulas podem ser utilizadas para separar múltiplas
	// expressões na mesma declaração `case`. A utilização
	// de `default` é opcional.
	switch time.Now().Weekday() {
	case time.Saturday, time.Sunday:
		fmt.Println("É fim de semana")
	default:
		fmt.Println("É dia de semana")
	}

	// `switch` sem nenhuma expressão é um meio alternativo
	// para representar a lógica if/else. Aqui também é exibido como
	// as expressões `case` podem ser não constantes.
	t := time.Now()
	switch {
	case t.Hour() < 12:
		fmt.Println("É antes do meio-dia")
	default:
		fmt.Println("É depois do meio-dia")
	}

	// Um `switch` de tipos compara tipos ao invés de valores.
	// É possível utilizá-lo para descobrir o valor de um tipo
	// interface.  Neste exemplo, a variável `t` terá o tipo
	// correspondente à sua cláusula.
	whatAmI := func(i interface{}) {
		switch t := i.(type) {
		case bool:
			fmt.Println("Sou um booleano")
		case int:
			fmt.Println("Sou um inteiro")
		default:
			fmt.Printf("Não sei o meu tipo %T\n", t)
		}
	}
	whatAmI(true)
	whatAmI(1)
	whatAmI("hey")
}
