// Go suporta _constantes_ de tipos strings, booleanos,
// e numericos.

package main

import (
	"fmt"
	"math"
)

// `const` é a palavra reservada para declarar uma constante.
const s string = "constant"

func main() {
	fmt.Println(s)

	// A declaração `const` pode aparecer em qualquer
	// lugar que a declaração `var` também possa.
	const n = 500000000

	// Expressões com constantes são performadas
	// aritmeticamente com precisão arbitrária.
	const d = 3e20 / n
	fmt.Println(d)

	// Uma constante numérica não possui um tipo
	// até que seja atribuído um, como uma conversão explícita.
	fmt.Println(int64(d))

	// Um tipo pode ser implicitamente atribuído a uma constante
	// numérica ao usá-la num contexto que requer um tipo,
	// como atribuição a uma variável ou uma chamada de função.
	// Por exemplo, a função `math.Sin` espera um valor de tipo
	// `float64`.
	fmt.Println(math.Sin(n))
}
