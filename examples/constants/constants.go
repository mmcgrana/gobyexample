// Go ammette l'utilizzo di _costanti_ di tipo string, boolean,
// e di tipo numerico

package main

import "fmt"
import "math"

// La keyword `const` viene utilizzata per dichiarare una costante
const s string = "constant"

func main() {
    fmt.Println(s)

    // La keyword `const` può essere utilizzata ovunuque
    // la keyword `var` è ammessa
    const n = 500000000

    // Le espressioni costanti vengono calcolate in aritmetica
    // a precisione arbitraria
    const d = 3e20 / n
    fmt.Println(d)

    // Una costante numerica non ha un tipo fin quando non gli
    // viene assegnato esplicitamente, ad esempio tramite un cast.
    fmt.Println(int64(d))

    // Per assegnare un tipo ad una costante di tipo numerico
    // si può anche utilizzare la constante in un contesto che richiede
    // un tipo, quali un assegnamento od una chiamata di funzione.
    // In questo caso `math.Sin` si aspetta un valore di tipo `float64`.
    fmt.Println(math.Sin(n))
}
