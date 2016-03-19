// Le [funzioni variadiche](https://it.wikipedia.org/wiki/Funzione_variadica) possono essere chiamate con un numero
// arbitrario di parametri in coda. `fmt.Println` è il classico esempio
// di una funzione variadica.

package main

import "fmt"

// Questa è un esempio di funzione che accetta un numero
// arbitrario di parametri di tipo `int`.
func sum(nums ...int) {
    fmt.Print(nums, " ")
    total := 0
    for _, num := range nums {
        total += num
    }
    fmt.Println(total)
}

func main() {

    // Le funzioni variadiche possono essere invocate nel
    // classico modo indicando ogni parametro separatamente
    sum(1, 2)
    sum(1, 2, 3)

    // Se i parametri si trovano dentro uno slice, puoi passarlo
    // direttamente ad una funzione variadica tramtite la sintassi
    // seguente: `nomefunzione(slice...)`
    nums := []int{1, 2, 3, 4}
    sum(nums...)
}
