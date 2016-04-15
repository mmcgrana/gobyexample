// Le _Funzioni_ svolgono un ruolo fondamentale in Go.
// Capiremo come usare le funzioni tramite una serie di esempi

package main

import "fmt"

// Questa è una funzione che accetta due parametri
// di tipo `int` e ritorna la loro somma (sempre di tipo `int`).
func plus(a int, b int) int {

    // Go non ritornerà il valore dell'ultima espressione: se
    // bisogna ritornare un valore, lo si deve ritornare
    // esplicitamente con il comando `return`
    return a + b
}

// Nelle funzioni con parametri multipli dello stesso tipo
// si può omettere il tipo per i parametri consecutivi che
// hanno lo stesso tipo, e indicare il tipo solo per
// l'ultimo parametro.
func plusPlus(a, b, c int) int {
    return a + b + c
}

func main() {

    // Puoi chiamare una funzione con la classica
    // sintassi `nomefunzione(parametri)`.
    res := plus(1, 2)
    fmt.Println("1+2 =", res)

    res = plusPlus(1, 2, 3)
    fmt.Println("1+2+3 =", res)
}
