// Go supporta le [Funzioni ricorsive](https://it.wikipedia.org/wiki/Algoritmo_ricorsivo)
// Ecco il classico esempio di una funzione ricorsiva, il fattoriale.

package main

import "fmt"

// Questa funzione `fact` invoca se stessa fin quando
// non raggiunge il caso base per `n` uguale a `0`.
func fact(n int) int {
    if n == 0 {
        return 1
    }
    return n * fact(n-1)
}

func main() {
    fmt.Println(fact(7))
}
